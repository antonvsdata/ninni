import sys
import argparse
import csv
import datetime
import time
import psycopg2
import string

# Opening the connection to the database
conn_string = "host = 'biodb.uef.fi' dbname = 'antom' user = 'antom' password = 'd0189244be'"
dbconn = psycopg2.connect(conn_string)
cursor = dbconn.cursor()

#SQL statements used in the script:

add_assoc = ("INSERT INTO associations "
             "(dataset_id, effect, effect_l95, effect_u95, n, p, p_fdr)"
             "VALUES(%s, %s, %s, %s, %s, %s, %s)")
assocquery = ("SELECT * FROM associations WHERE dataset_id = %s AND effect = %s AND effect_l95 = %s"
              "AND effect_u95 = %s AND n = %s AND p = %s AND p_fdr = %s")

add_var = ("INSERT INTO variables (label, description) VALUES(%s, %s)")
update_var = ("UPDATE variables SET description = %s WHERE label = %s")
varquery = ("SELECT * FROM variables WHERE label=%s")

add_meta = ("INSERT INTO datasetmetadata (label, description) VALUES (%s, %s)")
metaquery = ("SELECT * FROM datasetmetadata WHERE label = %s")

add_metatodata = ("INSERT INTO datasettometadata (dataset_id, datasetmetadata_id) VALUES (%s, %s)")

add_assoctovar = ("INSERT INTO associationtovariable (association_id, variable_id) VALUES (%s, %s)")

add_dataset = ("INSERT INTO Datasets (label, description, varnum, effect_type) VALUES (%s, %s, %s, %s)")
add_rowcount = ("UPDATE Datasets SET rowcount = %s WHERE id = %s")
datasetquery = ("SELECT * FROM datasets WHERE label=%s")


def parse_metadata(file):
    print "\n"
    with open(file,'r') as metafile:
        rdr = csv.DictReader(metafile, delimiter = "\t")
        print("Inserting metadata...")
        for row in rdr:
            metadata = (row["LABEL"],row["DESCRIPTION"])
            cursor.execute(add_meta, metadata)

        print"Metadata inserted"

def get_metaid(meta_label):
    cursor.execute(metaquery, (meta_label,))
    meta_row = cursor.fetchone()
    if(meta_row is not None):
        return meta_row[0]
    else:
        return None

def get_datasetid(dataset_label):
    cursor.execute(datasetquery, (dataset_label,))
    dataset_row = cursor.fetchone()
    if(dataset_row is not None):
        return dataset_row[0]
    else:
        return None

def parse_metatodata(dsid, tagstr):
    print("Parsing metadata tags...")
    tags = tagstr.split(';')
    count = 0
    for tag in tags:
        metaid = get_metaid(tag)
        if (metaid is not None):
            cursor.execute(add_metatodata, (dsid, metaid))
            count += 1
    print count, " tag(s) associated with the dataset"



def parse_variables(file):
    print ("Parsing variable descriptions...")
    with open(file,'r') as varfile:
        rdr = csv.DictReader(varfile, delimiter = ",")
        print("Inserting variables...")
        count = 0
        for row in rdr:
            cursor.execute(update_var, (row["DESCRIPTION"], row["LABEL"]))
            count += 1

    print"Description of ", count, "variables updated"
    



# inserts variable into the table in the database,                  
# sets decription identical to label, returns variable id


def create_varid(var_label, depth=0):
    cursor.execute(varquery, (var_label,))
    var_row = cursor.fetchone()
    if(var_row is not None):
        return var_row[0]
    else:
    	cursor.execute(add_var, (var_label,var_label,))
    	dbconn.commit()
    	cursor.execute(varquery, (var_label,))
    	var_row = cursor.fetchone()
    	if(var_row is not None):
            return var_row[0]
    	else:
            return None

def parse_assoctovar(assoc_id, var_label):
    varid = create_varid(var_label)
    if (varid is not None):
        cursor.execute(add_assoctovar, (assoc_id, varid))

def get_associd(dsid,effect,effect_l,effect_u,n,p,p_fdr):
    cursor.execute(assocquery, (dsid,effect,effect_l,effect_u,n,p,p_fdr))
    assoc_row = cursor.fetchone()
    if(assoc_row is not None):
        return assoc_row[0]
    else:
        return None

# Adds associations to the database, checks if the dataset has one or two variables and acts accordingly
# maxlines parameter is only for testing
def parse_assoc(file, dsid, varnum, maxlines=-1):
    with open(file, 'rt') as csvfile:
        print("Parsing associations...")
        rdr = csv.DictReader(csvfile,delimiter=',')
        rowcount = 0
        for row in rdr:
            data_assoc = (dsid, row["EFFECT"],row["EFFECT_L95"], row["EFFECT_U95"],row["N"],row["P"],row["P_FDR"])
            cursor.execute(add_assoc, data_assoc)
            assoc_id = get_associd(dsid, row["EFFECT"],row["EFFECT_L95"], row["EFFECT_U95"],row["N"],row["P"],row["P_FDR"])
            parse_assoctovar(assoc_id,row["VARIABLE1_LABEL"])
            if (int(varnum) == 2):
                parse_assoctovar(assoc_id,row["VARIABLE2_LABEL"])
            rowcount += 1
            if(maxlines > 0 and rdr.line_num >= maxlines):
                break
            if(rdr.line_num % 1000 == 0):
                dbconn.commit()
                print("{0} rows parsed".format(rowcount))
        
	cursor.execute(add_rowcount, (rowcount, dsid))
        print("{0} rows parsed and inserted".format(rowcount))

# Reads the datasets file, populates the Datasets table and calls other functions to populate
# the other tables
def parse_datasets(file):
    with open(file,'r') as dataset_file:
        rdr = csv.DictReader(dataset_file, delimiter = "\t")
        for row in rdr:
            print "\n"
            dataset = (row["LABEL"],row["DESCRIPTION"],row["VARNUM"],row["EFFECT_TYPE"])
            print "Importing dataset ", row["DATASET_FILENAME"], "..."
            cursor.execute(add_dataset, dataset)
            datasetid = get_datasetid(row["LABEL"])
            if (datasetid is not None):
                print("Dataset information inserted")
                parse_assoc(row["DATASET_FILENAME"], datasetid,row["VARNUM"])
                parse_metatodata(datasetid, row["METADATA_LABELS"])
            if (row["VARIABLES_FILENAME"] != 'None'):
                parse_variables(row["VARIABLES_FILENAME"])
            
def main():
    file = None
    maxlines = None
    parser = argparse.ArgumentParser()
    parser.add_argument("-dsf", "--dataset_file", help="The file containing information on the datasets to import")
    parser.add_argument("-mdf","--meta_data_file", help="The file containing the metadata for the datasets")


    args = parser.parse_args()

    if args.meta_data_file:
            parse_metadata(args.meta_data_file)

    if args.dataset_file:
            parse_datasets(args.dataset_file)
                         
    dbconn.commit()
    cursor.close()
    dbconn.close()

if __name__ == "__main__":
    main()
