# -*- coding: utf-8 -*-
"""
Migration functions for specific field types.
"""

from zope import component
from zope.intid import interfaces as intid_ifaces

from z3c import relationfield

from Products.CMFPlone.utils import safe_unicode, safe_hasattr
from plone.event.utils import default_timezone
from plone.namedfile.file import NamedBlobFile
from plone.namedfile.file import NamedBlobImage
from plone.app.textfield.value import RichTextValue

from Products.Archetypes.interfaces import referenceable

import logging
import pytz

logger = logging.getLogger(__name__)


def migrate_simplefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """Migrate a generic simple field.

    Copies the value of a Archetypes-object to a attribute of the same name
    to the target-object. The only transform is a safe_unicode of the value.
    """
    field = src_obj.getField(src_fieldname)
    try:
        if field:
            # Check for existence first
            field.getStorage(src_obj).get(field.getName(), src_obj)
            at_value = field.get(src_obj)
        else:
            at_value = getattr(src_obj, src_fieldname)
            if at_value and hasattr(at_value, '__call__'):
                at_value = at_value()
    except AttributeError:
        # The value is "unset" on the old instance and so should be unset on
        # the new instance
        pass
    else:
        if isinstance(at_value, tuple):
            at_value = tuple(safe_unicode(i) for i in at_value)
        if isinstance(at_value, list):
            at_value = [safe_unicode(i) for i in at_value]
        setattr(dst_obj, dst_fieldname, safe_unicode(at_value))


def migrate_richtextfield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """
    migrate a rich text field.
    This field needs some extra stuffs like keep the same mimetype.
    """
    field = src_obj.getField(src_fieldname)
    raw_text = ''
    if field:
        mime_type = field.getContentType(src_obj)
        raw_text = safe_unicode(field.getRaw(src_obj))
    else:
        at_value = getattr(src_obj, src_fieldname, None)
        if at_value:
            mime_type = at_value.mimetype
            raw_text = safe_unicode(at_value.raw)

    if raw_text.strip() == '':
            return
    richtext = RichTextValue(raw=raw_text, mimeType=mime_type,
                             outputMimeType='text/x-html-safe')
    setattr(dst_obj, dst_fieldname, richtext)


def migrate_imagefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """
    migrate an image field.
    This field needs to be migrated with an NamedBlobImage instance.
    """
    # get old image data and filename
    field = src_obj.getField(src_fieldname)
    accessor = field.getAccessor(src_obj)
    old_image = accessor()
    if old_image == '':
        return
    filename = safe_unicode(old_image.filename)
    old_image_data = old_image.data
    if safe_hasattr(old_image_data, 'data'):
        old_image_data = old_image_data.data

    # create the new image field
    namedblobimage = NamedBlobImage(data=old_image_data,
                                    filename=filename)

    # set new field on destination object
    setattr(dst_obj, dst_fieldname, namedblobimage)

    # handle a possible image caption field
    # postulate is the old caption field name is ending by 'Caption'
    # and the new field name is ending by '_caption'
    # is this postulate correct ?
    # should this field not be handle by itself because it will appear in the
    # old field list ?
    caption_field = src_obj.getField('%sCaption' % src_fieldname, None)
    if caption_field:
        setattr(dst_obj,
                ('%s_caption' % dst_fieldname),
                safe_unicode(caption_field.get(src_obj)))

    logger.info("Migrating image %s" % filename)


def migrate_blobimagefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """
    migrate an image field.
    Actually this field needs only to copy the existing NamedBlobImage instance
    to the new dst_obj, but we do some more in detail and create new fields
    """
    old_image = getattr(src_obj, src_fieldname)
    if old_image == '':
        return
    filename = safe_unicode(old_image.filename)
    old_image_data = old_image.data
    if safe_hasattr(old_image_data, 'data'):
        old_image_data = old_image_data.data
    namedblobimage = NamedBlobImage(data=old_image_data,
                                    filename=filename)

    # set new field on destination object
    setattr(dst_obj, dst_fieldname, namedblobimage)

    # handle a possible image caption field
    old_image_caption = getattr(src_obj, '%s_caption' % src_fieldname, None)
    if old_image_caption:
        setattr(dst_obj,
                ('%s_caption' % dst_fieldname),
                safe_unicode(old_image_caption))

    logger.info("Migrating image %s" % filename)


def migrate_filefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """
    migrate a file field.
    This field needs to be migrated with an NamedBlobFile instance.
    """
    old_file = src_obj.getField(src_fieldname).get(src_obj)
    if old_file == '':
        return
    filename = safe_unicode(old_file.filename)
    old_file_data = old_file.data
    if safe_hasattr(old_file_data, 'data'):
        old_file_data = old_file_data.data
    namedblobfile = NamedBlobFile(
        contentType=old_file.content_type,
        data=old_file_data,
        filename=filename)
    setattr(dst_obj, dst_fieldname, namedblobfile)
    logger.info("Migrating file %s" % filename)


def migrate_datetimefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """Migrate a datefield."""
    old_value = src_obj.getField(src_fieldname).get(src_obj)
    if old_value == '':
        return
    if src_obj.getField('timezone', None) is not None:
        old_timezone = src_obj.getField('timezone').get(src_obj)
    else:
        old_timezone = default_timezone(fallback='UTC')
    new_value = datetime_fixer(old_value.asdatetime(), old_timezone)
    setattr(dst_obj, dst_fieldname, new_value)


def datetime_fixer(dt, zone):
    timezone = pytz.timezone(zone)
    if dt.tzinfo is None:
        return timezone.localize(dt)
    else:
        return timezone.normalize(dt)


def migrate_referencefield(src_obj, dst_obj, src_fieldname, dst_fieldname):
    """
    Migrate a ReferenceField to a plone.app.relationfield.
    """
    src_field = src_obj.getField(src_fieldname)
    # The references have already been migrated,
    # so get them from the new content
    src_refable = referenceable.IReferenceable(src_obj, None)
    src_targets = src_field.get(src_refable)
    if src_targets is src_field.getDefault(src_obj):
        return
    if not src_field.multiValued:
        src_targets = [src_targets]

    intids = component.queryUtility(intid_ifaces.IIntIds)
    dst_relations = []
    for src_target in src_targets:
        dst_relations.append(
            relationfield.RelationValue(intids.getId(src_target)))
        src_refable.deleteReference(referenceable.IReferenceable(src_target))
    if not src_field.multiValued:
        dst_relations, = dst_relations

    setattr(dst_obj, dst_fieldname, dst_relations)


# This mapping is needed to get the right migration method
# we use the full field type path as it is retrieved from the target-field
# (field.getType()), to avoid conflict.
# TODO In the __future__ we should have a more dynamic way to configure this
# mapping
FIELDS_MAPPING = {'RichText': migrate_richtextfield,
                  'NamedBlobFile': migrate_filefield,
                  'NamedBlobImage': migrate_imagefield,
                  'Datetime': migrate_datetimefield,
                  'Date': migrate_datetimefield,
                  'Relation': migrate_referencefield,
                  'RelationList': migrate_referencefield,
                  'RelationChoice': migrate_referencefield}
