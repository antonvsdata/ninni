import sys
import argparse
import csv
import datetime
import time
import psycopg2
import string
import os.path

# Retrieving database configuration
config_file = 'database_import.config-TEST'
infos = []
with open(config_file) as tsv:
    for line in csv.reader(tsv, delimiter = '\t'):
        infos.append(line[1])

if len(infos) == 4:
    conn_string = "host = '" + infos[0] + "' dbname = '" + infos[1] + "' user = '" + infos[2] + "' password = '" + infos[3] + "'"
elif len(infos) == 5:
    conn_string = "host = '" + infos[0] + "' port = '" + infos[1] + "' dbname = '" + infos[2] + "' user = '" + infos[3] + "' password = '" + infos[4] + "'"

# Opening the connection to the database
dbconn = psycopg2.connect(conn_string)
cursor = dbconn.cursor()

#SQL statements used in the script:

add_assoc = ("INSERT INTO associations "
             "(ID, dataset_id, effect, effect_l95, effect_u95, n, p, p_fdr)"
             "VALUES(%s, %s, %s, %s, %s, %s, %s, %s)")
assocquery = ("SELECT * FROM associations WHERE dataset_id = %s AND effect = %s AND effect_l95 = %s"
              "AND effect_u95 = %s AND n = %s AND p = %s AND p_fdr = %s")

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
metavarquery = ("SELECT * FROM metavariables WHERE label= %s")
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

def import_metadata(file):
    global datasetMetaDataID
    print( "\n")
    with open(file,'r') as metafile:
        rdr = csv.DictReader(metafile, delimiter = ",")
        print("Inserting metadata...")
        for row in rdr:
            metadata = (datasetMetaDataID, row["LABEL"],row["DESCRIPTION"])
            cursor.execute(add_meta, metadata)
            datasetMetaDataID += 1

        print("Metadata inserted")

def get_metaid(meta_label):
    cursor.execute(metaquery, (meta_label,))
    meta_row = cursor.fetchone()
    if(meta_row is not None):
        return meta_row[0]
    else:
        return None

def import_metatodata(dsid, tagstr):
    global datasetToMetaDataID
    print("Parsing metadata tags...")
    tags = tagstr.split(';')
    count = 0
    for tag in tags:
        metaid = get_metaid(tag)
        if (metaid is not None):
            cursor.execute(add_metatodata, (datasetToMetaDataID, dsid, metaid))
            datasetToMetaDataID += 1
            count += 1
    print(count, " tag(s) associated with the dataset")



def import_variables(file):
    print ("Parsing variable descriptions...")
    with open(file,'r') as varfile:
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
    if(var_row is not None):
        varID = var_row[0]
    else:
        cursor.execute(add_var, (variableID, var_label,var_label))
        dbconn.commit()
        varID = variableID
        variableID += 1
    return varID

def import_assoctovar(assoc_id, var_label):
    global associationToVariableID
    varid = create_varid(var_label)
    if (varid is not None):
        cursor.execute(add_assoctovar, (associationToVariableID, assoc_id, varid))
        associationToVariableID += 1


def isfloat(value):
    try:
        float(value)
        return True
    except ValueError:
        return False
    
# Adds associations to the database, checks if the dataset has one or two variables and acts accordingly
# maxlines parameter is only for testing
def import_associations(file, dsid, varnum, maxlines=10):
    global metavariableID, associationID, strvalID, numvalID
    with open(file, 'rt') as csvfile:
        print("Parsing associations...")
        rdr = csv.DictReader(csvfile,delimiter=',')
        # Save first row separately to check the number of columns
        first_row = next(rdr)
        csvfile.seek(0)
        next(rdr) 
        ncol = len(first_row)
        if varnum == 1:
            col_limit = 7
        else:
            col_limit = 8  
        # Check if there are extra columns (metavariables)
        # If yes, populate metavariables table and save info about metavariables to a 2D-table
        if ncol > col_limit:
            metavarnum = ncol - col_limit
            metavars = [None] * metavarnum
            for i in range(0,metavarnum):
                metavars[i] = [None] * 3
                metavars[i][0] = rdr.fieldnames[i+col_limit]
                metavars[i][1] = metavariableID
                if isfloat(first_row[metavars[i][0]]):
                    metavars[i][2] = "num"
                else:
                    metavars[i][2] = "str"
                cursor.execute(add_metavar, (metavariableID, rdr.fieldnames[i+col_limit]))
                metavariableID +=1
            dbconn.commit()
        rowcount = 0
        for row in rdr:
            data_assoc = (associationID, dsid, row["EFFECT"],row["EFFECT_L95"], row["EFFECT_U95"],row["N"],row["P"],row["P_FDR"])
            cursor.execute(add_assoc, data_assoc)
            import_assoctovar(associationID,row["VARIABLE1_LABEL"])
            if (int(varnum) == 2):
                import_assoctovar(associationID, row["VARIABLE2_LABEL"])
            if ncol > col_limit:
                for i in range(0,metavarnum):
                    if metavars[i][2] == "num":
                        cursor.execute(add_numval,(numvalID, row[metavars[i][0]],associationID,metavars[i][1]))
                        numvalID += 1
                    if metavars[i][2] == "str":
                        cursor.execute(add_strval,(strvalID, row[metavars[i][0]],associationID,metavars[i][1]))
                        strvalID += 1
            rowcount += 1
            associationID += 1
            if(maxlines > 0 and rowcount >= maxlines):
                break
            if(rdr.line_num % 1000 == 0):
                dbconn.commit()
                print("{0} rows imported".format(rowcount))

        cursor.execute(add_rowcount, (rowcount, dsid))
        print("{0} rows imported".format(rowcount))

# Read the datasets file, populate the Datasets table and calls other functions to populate
# the other tables
def import_datasets(file):
    global datasetID
    with open(file,'r') as dataset_file:
        rdr = csv.DictReader(dataset_file, delimiter = ",")
        for row in rdr:
            print("\n")
            dataset = (datasetID, row["LABEL"],row["DESCRIPTION"],row["VARNUM"],row["EFFECT_TYPE"])
            print("Importing dataset ", row["DATASET_FILENAME"], "...")
            cursor.execute(add_dataset, dataset)
            print("Dataset information inserted")
            import_associations(row["DATASET_FILENAME"], datasetID,row["VARNUM"])
            import_metatodata(datasetID, row["METADATA_LABELS"])
            if (row["VARIABLES_FILENAME"] is not None and row["VARIABLES_FILENAME"] != ""):
                import_variables(row["VARIABLES_FILENAME"])
            datasetID += 1

class FileNotFoundError(Exception):
    def __init__(self,file):
        self.file = file

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

def set_table_ids():
    global datasetID, associationID, datasetToMetaDataID, datasetMetaDataID, associationToVariableID, variableID, numvalID, strvalID, metavariableID
    datasetID = get_last_id("datasets")
    associationID =  get_last_id("associations")
    datasetToMetaDataID = get_last_id("datasettometadata")
    datasetMetaDataID = get_last_id("datasetmetadata")
    associationToVariableID = get_last_id( "associationtovariable")
    variableID = get_last_id("variables")
    numvalID = get_last_id("numval")
    strvalID = get_last_id("strval")
    metavariableID = get_last_id("metavariables")
        

def get_last_id(table):
    query = "SELECT id FROM " + table + " ORDER BY id DESC LIMIT 1;"
    cursor.execute(query)
    last_row = cursor.fetchone()
    if last_row is not None:
        last_id = last_row[0]
    else:
        last_id = 1
    return last_id
    
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
    file = None
    maxlines = None
    parser = argparse.ArgumentParser()
    parser.add_argument("-dsf", "--dataset_file", help="The file containing information on the datasets to import")
    parser.add_argument("-mdf","--meta_data_file", help="The file containing the metadata for the datasets")
    parser.add_argument("-a","--append", action = 'store_true', help="Add dataset to the databse instead of clearing the database before import")
        
    args = parser.parse_args()
    
    if not args.append:
		# Drop and recreate the database schema
        executeSQLFromFile("drop_schema.sql")
        print("Database schema dropped")
        executeSQLFromFile("create_schema.sql")
        print("Database schema recreated")
        
    if args.append:
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
        set_table_ids()
        
    #If args.append: Set all IDs to last IDs in database
        

    if args.meta_data_file:
        import_metadata(args.meta_data_file)

    if args.dataset_file:
        try:
            check_files_exist(args.dataset_file)
            import_datasets(args.dataset_file)
        except FileNotFoundError as e:
            print("File not found:", e.file)
                         
    dbconn.commit()
    cursor.close()
    dbconn.close()

if __name__ == "__main__":
    main()
