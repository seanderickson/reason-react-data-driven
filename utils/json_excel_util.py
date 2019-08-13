import argparse
import json
import logging
import os
import xlrd
import xlsxwriter
import six
from collections import defaultdict, OrderedDict

DEBUG = True

logger = logging.getLogger(__name__)

####
# Converter Utility for application metadata,
# 
# JSON to XLSX, XLSX to JSON:
#
# - read db.json file into xlsx workbook: see "from_json"
#
# - read db.xlxs file into JSON: see "from_xlsx"
#
# NOTE: The "Field" schema is hardcoded; 
# data types for conversion are entered manually, see the "SCHEMA" section.
# 
####

class SCHEMA(object):

  class TABLES(object):
    
    FIELD = 'field'
    DERIVED_FIELD = 'derived_field'
    RESOURCE = 'resource'
    VOCAB = 'vocabulary'

  class VOCAB(object):

    class data_type(object):
      
      STRING = 'string'
      INTEGER = 'integer'
      FLOAT = 'float'
      BOOLEAN = 'boolean'
      ARRAY_STRING = 'arraystring'
      ARRAY_INT = 'arrayint'
      
  class FIELD(object):
   # Predefined Meta Field schema

    resource_name = 'field'

    NAME = 'name'
    RESOURCE_NAME = 'resource_name'
    ORDINAL = 'ordinal'
    FIELD_REF = 'field_ref'
    TITLE = 'title'
    DESCRIPTION = 'description'
    DATA_TYPE = 'data_type'
    DISPLAY_TYPE = 'display_type'
    EDITABLE = 'editable'
    EDIT_TYPE = 'edit_type'
    VOCAB_SCOPE = 'vocab_scope'
    REF_ENDPOINT = 'ref_endpoint'
    HREF_TEMPLATE = 'href_template'
    VALIDATORS = 'validators'
    LIST_ORDINAL = 'list_ordinal'
    DEFAULT = 'default'

    identity_fields = [RESOURCE_NAME, NAME, ORDINAL]

    final_required_fields = identity_fields + [DATA_TYPE, TITLE]

  FIELD.data_types = {
      FIELD.NAME: VOCAB.data_type.STRING,
      FIELD.RESOURCE_NAME: VOCAB.data_type.STRING,
      FIELD.ORDINAL: VOCAB.data_type.INTEGER,
      FIELD.FIELD_REF: VOCAB.data_type.STRING,
      FIELD.TITLE: VOCAB.data_type.STRING,
      FIELD.DESCRIPTION: VOCAB.data_type.STRING,
      FIELD.DATA_TYPE: VOCAB.data_type.STRING,
      FIELD.DISPLAY_TYPE: VOCAB.data_type.STRING,
      FIELD.EDITABLE: VOCAB.data_type.BOOLEAN,
      FIELD.EDIT_TYPE: VOCAB.data_type.STRING,
      FIELD.VOCAB_SCOPE: VOCAB.data_type.STRING,
      FIELD.REF_ENDPOINT: VOCAB.data_type.STRING,
      FIELD.HREF_TEMPLATE: VOCAB.data_type.STRING,
      FIELD.VALIDATORS: VOCAB.data_type.ARRAY_STRING,
      FIELD.LIST_ORDINAL: VOCAB.data_type.INTEGER,
      FIELD.DEFAULT: VOCAB.data_type.STRING,
    }
 
  class RESOURCE(object):

    resource_name = 'resource'
    
parser = argparse.ArgumentParser(
  description='Read the json metadata file to xls and write from xls to json')
parser.add_argument(
    '-f', '--input_file', required=True,
    help='''input file''')
parser.add_argument(
    '-of', '--output_file', required=True,
    help='''output file''')
parser.add_argument(
    '-v', '--verbose', dest='verbose', action='count',
    help='Increase verbosity (specify multiple times for more)')

def from_json(input_file, output_filename):
  '''
  Read in a JSON file of the format:
  {
    "resource1_name": [
      { row dict of data},
      { row dict of data},
      ...
    ],
    "resource2_name": [
      { row dict of data},
      { row dict of data},
      ...
    ],
    ...
  }

  Output a workbook having the structure:
  - workbook sheet names are resource names
  - sheets are data for the given resource
  - column titles are field names

  TODO: validation is not performed

  '''
  FIELD = SCHEMA.FIELD

  input_data = json.loads(input_file.read())

  if SCHEMA.TABLES.FIELD not in input_data:
    raise Exception(
      'Required "%r" resource entry not found' % SCHEMA.TABLES.FIELD)

  fields = OrderedDict()

  for field in input_data[SCHEMA.TABLES.FIELD]:
    resource_name = field[FIELD.RESOURCE_NAME]
    if resource_name not in fields:
      fields[resource_name] = OrderedDict()
    fields[resource_name][field[FIELD.NAME]] = field

  # Order resources
  if 'resource' not in input_data:
    raise Exception('Required "resource" element not found')
  sorted_resources = sorted(
    [(r['id'],r[FIELD.NAME]) for r in input_data['resource']])

  logger.info('field resources: %r, %d', fields.keys(), len(fields) )
  logger.info('field fields: %r', fields[SCHEMA.TABLES.FIELD].keys())

  wb = xlsxwriter.Workbook(output_filename, {'constant_memory': True})

  # Write the Field metadata

  target_resource = FIELD.resource_name
  sheet = wb.add_worksheet(target_resource)

  def fieldEncode(field_name, v):
    val = v
    if field_name == FIELD.EDITABLE:
      if v is True:
        val = 'true'
      else:
        val = 'false'
    if val is None:
      val = ""
    if isinstance(val, (list,tuple)):
      val = ";".join(val)
    else:
      val = "{}".format(val)
    return val

  fieldRows = fields[target_resource].keys()
  sheet.write_row(0,0,fieldRows)
  current_row = 1
  for resource_name,fielddict in fields.items():
    for field_name,f in fielddict.items():
      row = []
      for k in fieldRows:
        row.append(fieldEncode(k,f.get(k)))
      if current_row == 1:
        logger.info('row: %r', row)
      sheet.write_row(current_row,0,row)
      current_row += 1

  # Write other entries are resource data to be written
  
  for id,resource_name in sorted_resources:
    if resource_name == FIELD.resource_name:
      continue
    if resource_name not in fields:
      logger.warn('resource has no field definitions: %r', resource_name)
      continue
    list_data = input_data[resource_name]
    
    sheet = wb.add_worksheet(resource_name)
    field_names = fields[resource_name].keys()
    sheet.write_row(0,0,field_names)
    current_row = 1
    for data in list_data:

      row = [fieldEncode(field_name, data.get(field_name, "")) 
        for field_name in field_names]
      sheet.write_row(current_row, 0, row)
      current_row += 1
  
  wb.close()
  logger.info('wrote: %r', output_filename)


### XLSX Read Utilities for xlrd

def read_string(cell):
    '''
    Read the cell as a string. 
    - empty strings are convervted to None
    - integer cells are read as strings
    '''
    value = cell.value
    logger.debug('read string: %r: %r', cell, value)
    if value is None:
        return None
    elif cell.ctype == xlrd.XL_CELL_NUMBER:
        ival = int(value)
        if value == ival:
            value = ival
        return str(value)
    else:
        if isinstance(value, six.string_types):
            value = value.strip()
            if not value:
                return None
        return value

def sheet_rows(workbook_sheet):
    def read_row(row):
        for col in range(workbook_sheet.ncols):
            logger.debug(
                'read cell: %r:%r, %r', row, col, 
                read_string(workbook_sheet.cell(row,col)))
            yield read_string(workbook_sheet.cell(row,col))
    for row in range(workbook_sheet.nrows):
        yield read_row(row)

def sheet_rows_dicts(sheet):
    '''
    Read the sheet as an array of dicts, with the first row as the dict keys
    '''
    rows = sheet_rows(sheet)
    header = [x for x in rows.next()]
    for row in rows:
        yield dict(zip(header,[x for x in row]))

def workbook_as_datastructure(workbook):
    '''
    Create an ordered dict of the sheets in the workbook:
    {
        sheet_name: iterable of sheet rows
    }
    '''
    workbook_datastructure = OrderedDict()
    for sheet_num in range(workbook.nsheets):
        sheet = workbook.sheet_by_index(sheet_num)
        workbook_datastructure[sheet.name] = sheet_rows_dicts(sheet)
    return workbook_datastructure

def from_xlsx(input_file, output_filename):
  '''
  Read in a workbook of schema data that was generated earlier by "from_json":
  - workbook sheet names are resource names
  - sheets are data for the given resource
  - column titles are field names

  Output a json file that is of the structure:
  {
    "resource1_name": [
      { row dict of data},
      { row dict of data},
      ...
    ],
    "resource2_name": [
      { row dict of data},
      { row dict of data},
      ...
    ],
    ...
  }
  '''
  FIELD = SCHEMA.FIELD
  DATA_TYPE = SCHEMA.VOCAB.data_type
  wb = xlrd.open_workbook(file_contents=input_file.read())
  workbook_ds = workbook_as_datastructure(wb)

  logger.info('found keys: %r',  workbook_ds.keys())

  # Pass 1: unspool the generators

  unspooled_data = {}
  def unspool_row(row):
    return {k:v for k,v in row.items() }
  for resource_name, rows in workbook_ds.items():
    output_rows = [unspool_row(x) for x in rows]
    unspooled_data[resource_name] = output_rows
  
  # Pass 2: Read the fields using predefined Meta Field schema

  def parse_string(v):
    val = v
    if val is None:
      return val
    val = "{}".format(val)
    val = val.strip()
    return val

  def parse_int(v):
    val = v
    if val is None:
      return val
    if isinstance(val, six.string_types):
        # Note: convert an integer from values like "5.0"
        try:
            val = int(float(val))
        except:
            val = int(val)
    else:
        val = int(val)
    return val

  def parse_float(v):
    val = v
    if val is None:
      return val
    val = float(val)
    return val

  def parse_boolean(v):
    val = v
    if val is None:
      return val
    if val is True or val is False:
      return val
    val = str(val).strip()
    if(val.lower() == 'true' 
      or val.lower() == 't' or val == '1'):
      return True
    return False    

  def parse_arraystring(v):
    if isinstance(v, (list,tuple)):
      # Already parsed (parent field)
      return v
    val = parse_string(v)
    if val is None:
      return val
    return val.split(";")
    
  def parse_arrayint(v):
    if isinstance(v, (list,tuple)):
      # Already parsed (parent field)
      return v
    val = parse_arraystring(v)
    if val is None:
      return val
    
    return [parse_int(lv) for lv in val]

  parsers_by_data_type = {
    DATA_TYPE.STRING: parse_string,
    DATA_TYPE.INTEGER: parse_int,
    DATA_TYPE.FLOAT: parse_float,
    DATA_TYPE.BOOLEAN: parse_boolean,
    DATA_TYPE.ARRAY_STRING: parse_arraystring,
    DATA_TYPE.ARRAY_INT: parse_arrayint,
  }

  def field_decode(data_type, v):
    return parsers_by_data_type[data_type](v)

  # Parse the Meta Field "field" sheet

  raw_fields = unspooled_data.get(SCHEMA.TABLES.FIELD)
  if not raw_fields:
    raise Exception('No "%r" definition sheet found' % SCHEMA.TABLES.FIELD)

  resource_field_dict = defaultdict(dict)
  parsed_fields = []
  for raw_field in raw_fields:
    parsed_field = {}
    for fieldkey, val in raw_field.items():
      data_type = FIELD.data_types.get(fieldkey)
      if data_type is None:
        raise Exception("Unknown field: {}: {}".format(fieldkey, raw_field) )
      parsed_field[fieldkey] = field_decode(data_type, val)

    # Verify identity_fields fields
    keys_null = set([k for k,v in parsed_field.items() if v is None])
    if set(FIELD.identity_fields) & keys_null:
      raise Exception(
        "derived field: {}, identity_fields must be defined: {}"
        .format(parsed_field, set(FIELD.identity_fields) & keys_null))

    resource_field_dict[parsed_field[FIELD.RESOURCE_NAME]]\
      [parsed_field[FIELD.NAME]] = parsed_field
    parsed_fields.append(parsed_field)

  # Check required fields

  for resource_name, field_dict in resource_field_dict.items():
    for field_name, field in field_dict.items():
      keys_null = set([k for k,v in field.items() if v is None])
      if set(FIELD.final_required_fields) & keys_null:
        raise Exception(
          "Resource: {}, meta field: {}, required fields must be defined: {}"
          .format(resource_name, field_name, 
                  set(FIELD.final_required_fields) & keys_null))

  unspooled_data[SCHEMA.TABLES.FIELD] = parsed_fields

  # Parse the Meta "derived_field" fields
  raw_derived_fields = unspooled_data.get('derived_field')
  if raw_derived_fields:

    derived_field_dict = defaultdict(dict)
    parsed_fields = []
    for raw_field in raw_derived_fields:
      
      # Verify identity_fields fields
      keys_null = set([k for k,v in raw_field.items() if v is None])
      if set(FIELD.identity_fields) & keys_null:
        raise Exception(
          "derived field: {}, identity_fields must be defined: {}"
          .format(parsed_field, set(FIELD.identity_fields) & keys_null))
      
      resource_name = raw_field[FIELD.RESOURCE_NAME]
      name = raw_field[FIELD.NAME]
      
      # Verify that the derived field does not shadow a non-derived field
      if resource_name in resource_field_dict:
        raise Exception(
          "Derived field definition conflict, already defined: {}:{}".format(
            resource_name, name))

      field_ref = raw_field.get('field_ref')
      if field_ref:
       
        # Verify that the derived field references a field
       
        parts = field_ref.split("/")
        if len(parts) != 2:
          raise Exception(
            'Derived field "field_ref" format is "resource_name/field_name":'
            ' {} in {}'.format(field_ref, raw_field))

        (parent_resource_name, parent_field_name) = parts
        if parent_resource_name not in resource_field_dict:
          raise Exception(
            'Derived field parent resource not found: {} in {}'.format(
            parent_resource_name, raw_field))
        parent_field = resource_field_dict[
          parent_resource_name].get(parent_field_name)
        if not parent_field:
          raise Exception('{}:{}: Derived field not found: {}, resources: {}, '
            'fields found: {}'.format(
              resource_name, name, field_ref, 
              resource_field_dict.keys(),
              resource_field_dict[parent_resource_name].keys()))

        # Override values with the parent field values

        for (propkey, propval) in parent_field.items():
          derived_field_override_val = raw_field.get(propkey)
          if derived_field_override_val is not None:
            if DEBUG:
              logger.info('Derived field val overrides parent field val: %r',
              (resource_name, name, propkey, propval, parent_field.get(propkey)))
          else:
            raw_field[propkey] = propval

      # Parse

      parsed_field = {}
      for fieldkey, val in raw_field.items():
        data_type = FIELD.data_types.get(fieldkey)
        if data_type is None:
          raise Exception("Unknown field: {}: {}".format(fieldkey, raw_field) )
        parsed_field[fieldkey] = field_decode(data_type, val)
      

      logger.info("parsed field: %r", parsed_field)
      derived_field_dict[parsed_field[FIELD.RESOURCE_NAME]]\
        [parsed_field[FIELD.NAME]] = parsed_field
      parsed_fields.append(parsed_field)

    # # Check for required Field fields

    for resource_name, field_dict in derived_field_dict.items():
      for field_name, field in field_dict.items():
        keys_null = set([k for k,v in field.items() if v is None])
        if set(FIELD.final_required_fields) & keys_null:
          raise Exception(
            "Resource: {}, meta field: {}, required fields must be defined: {}"
            .format(resource_name, field_name, 
                    set(FIELD.final_required_fields) & keys_null))
  
  # After parsing, add derived fields to the other fields
  unspooled_data[SCHEMA.TABLES.FIELD].extend(parsed_fields)

  # Parse the rest of the value dicts using the Meta Field definitions
  
  for resource_name in unspooled_data.keys():
    if resource_name in [
      SCHEMA.TABLES.FIELD,SCHEMA.TABLES.DERIVED_FIELD, 'notes']:
      continue
    
    raw_values = unspooled_data[resource_name]
    parsed_rows = []

    resource_field_schemas = resource_field_dict[resource_name]

    for row in raw_values:
      parsed_row = {}
      parsed_rows.append(parsed_row)
      for field_name, raw_val in row.items():

        field_schema = resource_field_schemas.get(field_name)
        if field_schema is None:
          raise Exception("Resource: {}, unknown field: {}".format(
            resource_name, field_name))
        
        parser = parsers_by_data_type.get(field_schema[FIELD.DATA_TYPE])
        if parser is None:
          raise Exception(
            "Resource: {}, field: {}, unknown data type: {}".format(
              resource_name, field_name, field_schema[FIELD.DATA_TYPE]))
        parsed_row[field_name] = parser(raw_val)
    unspooled_data[resource_name] = parsed_rows

  # Use ordinals to order the data
  ordered_data = OrderedDict()

  resources = sorted(
    [(d['id'],d[FIELD.NAME]) for d in unspooled_data['resource']])
  for id,resource in resources:
    rows = []
    ordered_data[resource] = rows

    fields = resource_field_dict[resource]
    fields_sorted = sorted(
      (f[FIELD.ORDINAL],f[FIELD.NAME]) for f in fields.values() )
  
    rows_of_dicts = unspooled_data.get(resource)
    if rows_of_dicts:
      for old_row in rows_of_dicts:
        new_row = OrderedDict()
        for ord,name in fields_sorted:
          new_row[name] = old_row.get(name)
        rows.append(new_row)

  # Write out parsed Excel data to JSON

  with open(output_filename, 'w') as of:
    of.write(json.dumps(ordered_data, indent=2))

  logger.info('wrote: %r', output_filename)



if __name__ == "__main__":
    args = parser.parse_args()
    log_level = logging.WARNING # default
    if args.verbose == 1:
        log_level = logging.INFO
    elif args.verbose >= 2:
        log_level = logging.DEBUG
        django_requests.DEBUG = True
    logging.basicConfig(
        level=log_level,
        format='%(msecs)d:%(module)s:%(lineno)d:%(levelname)s: %(message)s')

    with open(args.input_file) as input_file:

      _, extension = os.path.splitext(args.input_file)
      logger.info("Extension: %r", extension)
      # Case 1: input file is json
      if extension == ".json":
        from_json(input_file, args.output_file) 
      elif extension == ".xlsx":
        from_xlsx(input_file, args.output_file)
      else:
        raise Exception("Unknown file type {}".format(args.input_file))
