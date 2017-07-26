import sys
import argparse
import csv
import datetime
import time
import psycopg2
import string
import os.path

# Retrieving database configuration
config_file = 'database_import.config'
infos = []
with open(config_file) as tsv:
    for line in csv.reader(tsv, delimiter = '\t'):
        infos.append(line[1])
conn_string = "host = '" + infos[0] + "' port = '" + infos[1] + "' dbname = '" + infos[2] + "' user = '" + infos[3] + "' password = '" + infos[4] + "'"

# Opening the connection to the database
dbconn = psycopg2.connect(conn_string)
cursor = dbconn.cursor()

#SQL statements used in the script:
add_assoc = ("INSERT INTO associations "
             "(ID, dataset_id, effect, effect_l95, effect_u95, n, p, p_fdr)"
             "VALUES(%s, %s, %s, %s, %s, %s, %s, %s)")

add_var = ("INSERT INTO variables (ID, label, description) VALUES(%s, %s, %s)")
update_var = ("UPDATE variables SET description = %s WHERE label = %s")
varquery = ("SELECT * FROM variables WHERE label=%s")

add_meta = ("INSERT INTO datasetmetadata (ID, label, description) VALUES (%s, %s, %s)")
metaquery = ("SELECT * FROM datasetmetadata WHERE label = %s")

add_metatodata = ("INSERT INTO datasettometadata (ID, dataset_id, datasetmetadata_id) VALUES (%s, %s, %s)")

add_assoctovar = ("INSERT INTO associationtovariable (ID, association_id, variable_id) VALUES (%s, %s, %s)")

add_dataset = ("INSERT INTO Datasets (ID, label, description, varnum, effect_type) VALUES (%s, %s, %s, %s, %s)")
add_rowcount = ("UPDATE Datasets SET rowcount = %s WHERE id = %s")
datasetquery = ("SELECT * FROM datasets WHERE label=%s")

add_metavar = ("INSERT INTO metavariables (ID, label) VALUES (%s, %s)")
metavarquery = ("SELECT * FROM metavariables WHERE label = %s")
add_numval = ("INSERT INTO numval (ID, value, association_id, metavariable_id) VALUES (%s, %s, %s, %s)")
add_strval = ("INSERT INTO strval (ID, value, association_id, metavariable_id) VALUES (%s, %s, %s, %s)")

# Set all IDs to 1
datasetID = associationID = datasetToMetaDataID = datasetMetaDataID = associationToVariableID = variableID = numvalID = strvalID = metavariableID = 1


def executeSQLFromFile(filename):
    # Open and read the file as a single buffer
    fd = open(filename, 'r')
    sqlFile = fd.read()
    fd.close()

    # all SQL commands (split on ';')
    sqlCommands =  filter(None, sqlFile.strip('\n').split(';'))

    # Execute every command from the input file
    for command in sqlCommands:
        cursor.execute(command)


# Import dataset metadata from file
def import_metadata(filename):
    global datasetMetaDataID
    print( "\n")
    with open(filename,'r') as metafile:
        rdr = csv.DictReader(metafile, delimiter = ",")
        print("Inserting metadata...")
        for row in rdr:
            metadata = (datasetMetaDataID, row["LABEL"],row["DESCRIPTION"])
            cursor.execute(add_meta, metadata)
            datasetMetaDataID += 1

        print("Metadata inserted")


# Get ID of dataset metadata with know label
def get_metaid(meta_label):
    cursor.execute(metaquery, (meta_label,))
    meta_row = cursor.fetchone()
    if(meta_row is not None):
        return meta_row[0]
    else:
        return None


# Populate DatasetToMetaData table
# Link datasets to metadata tags
def import_datatometa(dsid, tagstr):
    global datasetToMetaDataID
    print("Linking to metadata tags...")
    tags = tagstr.split(';')
    count = 0
    for tag in tags:
        metaid = get_metaid(tag)
        if (metaid is not None):
            cursor.execute(add_metatodata, (datasetToMetaDataID, dsid, metaid))
            datasetToMetaDataID += 1
            count += 1
    print(count, " tag(s) associated with the dataset")


# Update description of variables
def update_variables(filename):
    with open(filename,'r') as varfile:
        rdr = csv.DictReader(varfile, delimiter = ",")
        print("Updating variables...")
        count = 0
        for row in rdr:
            cursor.execute(update_var, (row["DESCRIPTION"], row["LABEL"]))
            count += 1

    print("Description of ", count, "variables updated")
   
   
# inserts variable into the table in the database,                  
# sets decription identical to label, returns variable id
def create_varid(var_label):
    global variableID
    # Check if variable with the same label already exists in the database
    cursor.execute(varquery, (var_label,))
    var_row = cursor.fetchone()
    if var_row is not None:
        varID = var_row[0]
    else:
        cursor.execute(add_var, (variableID, var_label,var_label))
        dbconn.commit()
        varID = variableID
        variableID += 1
    return varID


# Populates the AssociationToVariable table
# Links associations to variables
def import_assoctovar(assoc_id, var_label):
    global associationToVariableID
    varid = create_varid(var_label)
    if (varid is not None):
        cursor.execute(add_assoctovar, (associationToVariableID, assoc_id, varid))
        associationToVariableID += 1
        
def import_metavariable(label):
    global metavariableID
    
    if metavar_row is None:
        cursor.execute(add_metavar, (metavariableID, label))
        metavariableID +=1

def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False
        
def isfloat_vector(v):
    all_float = True
    for value in v:
        if not isfloat(value):
            all_float = False
            break
    return all_float
   
# Read csv file into dict     
def read_to_dict(filename, maxlines):
        header = ''
        data = {}
        with open(filename) as f:
            rdr = csv.reader(f)
            header = next(rdr)
            for i in range(0,len(header)):
                data[header[i]] = []
            rowcount = 0
            for row in rdr:
                for i in range(0,len(row)):
                    data[header[i]].append(row[i])
                    rowcount += 1
                if (maxlines > 0 and rowcount >= maxlines):
                    break
        return data
    
# Adds associations to the database, checks if the dataset has one or two variables and acts accordingly
def import_associations(filename, dsid, varnum, maxlines):
    global metavariableID, associationID, strvalID, numvalID
    
    associations = read_to_dict(filename, maxlines)
    
    ncol = len(associations.keys())
    if varnum == 1:
        normal_columns = ["VARIABLE1", "EFFECT","EFFECT_L95","EFFECT_U95","N","P","P_FDR"]
    else:
        normal_columns = ["VARIABLE1_LABEL","VARIABLE2_LABEL", "EFFECT","EFFECT_L95","EFFECT_U95","N","P","P_FDR"]
    col_limit = len(normal_columns)
    
    # Check if there are extra columns (metavariables)
    # If yes, populate metavariables table and save info about metavariables to a 2D-table
    # First column: metavariable names, second column: type of variable, "num" or "str"
    if ncol > col_limit:
        # Get the names of metavariables
        metavar_names = list(set(associations.keys()) - set(normal_columns))
        metavarnum = ncol - col_limit
        metavars = [None] * metavarnum
        for i in range(0,metavarnum):
            metavars[i] = [None] * 3
            metavar_label = metavar_names[i]
            metavars[i][0] = metavar_label
            # If metavariable already exists in the database, get its ID
            cursor.execute(metavarquery, (metavar_label,))
            metavar_row = cursor.fetchone()
            if metavar_row is not None:
                metavars[i][1] = metavar_row[0]
            # If metavariable does not exist in the database, import it and save ID
            else:
                metavars[i][1] = metavariableID
                cursor.execute(add_metavar, (metavariableID, metavar_label))
                metavariableID +=1
            if isfloat_vector(associations[metavar_label]):
                metavars[i][2] = "num"
            else:
                metavars[i][2] = "str"
        dbconn.commit()
        
    rowcount = 0
    nrow = len(associations["EFFECT"])
    for i in range(0,nrow):
        data_assoc = (associationID, dsid, associations["EFFECT"][i],associations["EFFECT_L95"][i], associations["EFFECT_U95"][i], associations["N"][i], associations["P"][i], associations["P_FDR"][i])
        cursor.execute(add_assoc, data_assoc)
        import_assoctovar(associationID,associations["VARIABLE1_LABEL"][i])
        if varnum == 2:
            import_assoctovar(associationID, associations["VARIABLE2_LABEL"][i])
        if ncol > col_limit:
            for j in range(0,metavarnum):
                if metavars[j][2] == "num":
                    cursor.execute(add_numval,(numvalID, associations[metavars[j][0]][i],associationID,metavars[j][1]))
                    numvalID += 1
                if metavars[j][2] == "str":
                    cursor.execute(add_strval,(strvalID, associations[metavars[j][0]][i],associationID,metavars[j][1]))
                    strvalID += 1
        rowcount += 1
        associationID += 1
        if(maxlines > 0 and rowcount >= maxlines):
            break
        if(rowcount % 1000 == 0):
            dbconn.commit()
            print("{0} rows imported".format(rowcount))

    cursor.execute(add_rowcount, (rowcount, dsid))
    print("{0} rows imported".format(rowcount))


# Read the datasets file, populate the Datasets table and calls other functions to populate
# the other tables
def import_datasets(filename, maxlines):
    global datasetID
    with open(filename,'r') as dataset_file:
        rdr = csv.DictReader(dataset_file, delimiter = ",")
        for row in rdr:
            print("\n")
            dataset = (datasetID, row["LABEL"],row["DESCRIPTION"],row["VARNUM"],row["EFFECT_TYPE"])
            print("Importing dataset ", row["DATASET_FILENAME"], "...")
            cursor.execute(add_dataset, dataset)
            print("Dataset information inserted")
            import_associations(row["DATASET_FILENAME"], datasetID, int(row["VARNUM"]), maxlines)
            import_datatometa(datasetID, row["METADATA_LABELS"])
            if (row["VARIABLES_FILENAME"] is not None and row["VARIABLES_FILENAME"] != ""):
                update_variables(row["VARIABLES_FILENAME"])
            datasetID += 1


class FileNotFoundError(Exception):
    def __init__(self,filename):
        self.file = filename

# Check that all the files containing the associations and variables exist
# If not, throw a FileNotFoundError
def check_files_exist(dsfile):
    with open(dsfile,'r') as dataset_file:
        rdr = csv.DictReader(dataset_file, delimiter = ",")
        for row in rdr:
            df = row["DATASET_FILENAME"]
            if not os.path.isfile(df):
                raise FileNotFoundError(df)
            vf = row["VARIABLES_FILENAME"]
            if (row["VARIABLES_FILENAME"] is not None and row["VARIABLES_FILENAME"] != ""):
                if not os.path.isfile(vf):
                    raise FileNotFoundError(vf)
                    

# Get the next ID number for a table
# next ID = last ID in the table + 1
def get_next_id(table):
    query = "SELECT id FROM " + table + " ORDER BY id DESC LIMIT 1;"
    cursor.execute(query)
    last_row = cursor.fetchone()
    if last_row is not None:
        last_id = last_row[0]
    else:
        last_id = 0
    return last_id + 1

# Set IDs for all tables to the next ID
def set_table_ids():
    global datasetID, associationID, datasetToMetaDataID, datasetMetaDataID, associationToVariableID, variableID, numvalID, strvalID, metavariableID
    datasetID = get_next_id("datasets")
    associationID =  get_next_id("associations")
    datasetToMetaDataID = get_next_id("datasettometadata")
    datasetMetaDataID = get_next_id("datasetmetadata")
    associationToVariableID = get_next_id( "associationtovariable")
    variableID = get_next_id("variables")
    numvalID = get_next_id("numval")
    strvalID = get_next_id("strval")
    metavariableID = get_next_id("metavariables")
        

# Get labels of all datasets to be imported that already exist in the database
def datasets_exist(datasetfile):
    exist = []
    with open(datasetfile,'r') as dataset_file:
        rdr = csv.DictReader(dataset_file, delimiter = ",")
        for row in rdr:
            label = row["LABEL"]
            cursor.execute(datasetquery, (label,))
            dataset_row = cursor.fetchone()
            if dataset_row is not None:
                exist.append(label)
    return exist
            
            
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-dsf", "--dataset_file", help="The file containing information on the datasets to import")
    parser.add_argument("-mdf","--meta_data_file", help="The file containing the metadata for the datasets")
    parser.add_argument("-a","--append", action = 'store_true', help="Add dataset to the databse instead of clearing the database before import")
    parser.add_argument("-ml","--maxlines", default = -1, help = "Maximum number of lines imported per dataset (mainly for testing, defaults to unlimited)")
    
    args = parser.parse_args()
    
    if not args.append: # Database is cleared before import
        # Drop and recreate the database schema
        executeSQLFromFile("drop_schema.sql")
        print("Database schema dropped")
        executeSQLFromFile("create_schema.sql")
        print("Database schema recreated")
        
    if args.append: # New datasets will be appended to the database
        # If any of the datasets to be appended already exist in the database, stop the script
        existing_datasets = datasets_exist(args.dataset_file)
        if len(existing_datasets) > 0:
            print("Error: Following datasets already exist in the database:")
            for e in existing_datasets:
                print(e)
            print("Please only apend datasets that are not found in the database or remove --append to clear database before import")
            dbconn.commit()
            cursor.close()
            dbconn.close()
            sys.exit()
        # Set IDs for all the tables to next logical ID number
        set_table_ids()
        
    # Import dataset metadata
    if args.meta_data_file:
        import_metadata(args.meta_data_file)

    # Import datasets
    if args.dataset_file:
        try:
            check_files_exist(args.dataset_file)
            import_datasets(args.dataset_file, int(args.maxlines))
        except FileNotFoundError as e:
            print("File not found:", e.file)
    
    # Close connection to the database                     
    dbconn.commit()
    cursor.close()
    dbconn.close()

if __name__ == "__main__":
    main()
