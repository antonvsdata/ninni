
CREATE TABLE metavariables
(
  id INTEGER PRIMARY KEY NOT NULL,
  label VARCHAR(1000) NOT NULL
);


CREATE TABLE variables
(
  id INTEGER PRIMARY KEY NOT NULL,
  label VARCHAR(255),
  description VARCHAR(1000)
);


CREATE INDEX label_variables_idx
 ON variables
 ( label ASC );

CREATE INDEX description_variables_idx
 ON variables
 ( description ASC );

CREATE TABLE datasetmetadata
(
  id INTEGER PRIMARY KEY NOT NULL,
  label VARCHAR(255),
  description VARCHAR(1000)
);


CREATE INDEX label_datasetmetadata_idx
 ON datasetmetadata
 ( label ASC );

CREATE INDEX description_datasetmetadata_idx
 ON datasetmetadata
 ( description ASC );

CREATE TABLE datasets
(
  id INTEGER PRIMARY KEY NOT NULL,
  label VARCHAR(255),
  rowcount INTEGER,
  description VARCHAR
  (1000),
  varnum INTEGER,
  effect_type VARCHAR NOT NULL
);


  CREATE INDEX label_datasets_idx
 ON datasets
 ( label ASC );

  CREATE TABLE datasettometadata
  (
    id INTEGER PRIMARY KEY NOT NULL,
    datasetmetadata_id INTEGER NOT NULL,
    dataset_id INTEGER NOT NULL,
    FOREIGN KEY (datasetmetadata_id) REFERENCES datasetmetadata (id),
    FOREIGN KEY (dataset_id) REFERENCES datasets (id)
  );


  CREATE INDEX dataset_id_datasettometadata_idx
 ON datasettometadata
 ( dataset_id ASC );

  CREATE TABLE associations
  (
    id INTEGER PRIMARY KEY NOT NULL,
    dataset_id INTEGER NOT NULL,
    effect DOUBLE PRECISION,
    effect_l95 DOUBLE PRECISION,
    effect_u95 DOUBLE PRECISION,
    n INTEGER,
    p DOUBLE PRECISION,
    p_adj DOUBLE PRECISION,
    FOREIGN KEY (dataset_id) REFERENCES datasets (id)
  );


  CREATE INDEX n_idx
 ON associations
 ( n ASC );

  CREATE INDEX effect_idx
 ON associations
 ( effect ASC );

  CREATE INDEX p_adj_idx
 ON associations
 ( p_adj ASC );

  CREATE INDEX dataset_id_idx
 ON associations
 ( dataset_id ASC );

  CREATE TABLE strval
  (
    id INTEGER PRIMARY KEY NOT NULL,
    value VARCHAR(1000) NOT NULL,
    association_id INTEGER NOT NULL,
    metavariable_id INTEGER NOT NULL,
    FOREIGN KEY (metavariable_id) REFERENCES metavariables (id),
    FOREIGN KEY (association_id) REFERENCES associations (id)
  );


  CREATE TABLE numval
  (
    id INTEGER PRIMARY KEY NOT NULL,
    value DOUBLE PRECISION NOT NULL,
    association_id INTEGER NOT NULL,
    metavariable_id INTEGER NOT NULL,
    FOREIGN KEY (metavariable_id) REFERENCES metavariables (id),
    FOREIGN KEY (association_id) REFERENCES associations (id)
  );


  CREATE TABLE associationtovariable
  (
    id INTEGER PRIMARY KEY NOT NULL,
    association_id INTEGER NOT NULL,
    variable_id INTEGER NOT NULL,
    FOREIGN KEY (variable_id) REFERENCES variables (id),
    FOREIGN KEY (association_id) REFERENCES associations (id)
  );


  CREATE INDEX associationtovariable_idx
 ON associationtovariable
 ( variable_id ASC );

  CREATE INDEX associationtovariable_idx1
 ON associationtovariable
 ( association_id ASC );
