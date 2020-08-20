
CREATE TABLE public.Metavariables
(
  ID INTEGER NOT NULL,
  Label VARCHAR(1000) NOT NULL,
  CONSTRAINT metavariable_id_idx PRIMARY KEY (ID)
);


CREATE TABLE public.Variables
(
  ID INTEGER NOT NULL,
  Label VARCHAR(255),
  Description VARCHAR(1000),
  CONSTRAINT variables_id_idx PRIMARY KEY (ID)
);


CREATE INDEX label_variables_idx
 ON public.Variables
 ( Label ASC );

CREATE INDEX description_variables_idx
 ON public.Variables
 ( Description ASC );

CREATE TABLE public.DatasetMetaData
(
  ID INTEGER NOT NULL,
  Label VARCHAR(255),
  Description VARCHAR(1000),
  CONSTRAINT datasetmetadata_id_idx PRIMARY KEY (ID)
);


CREATE INDEX label_datasetmetadata_idx
 ON public.DatasetMetaData
 ( Label ASC );

CREATE INDEX description_datasetmetadata_idx
 ON public.DatasetMetaData
 ( Description ASC );

CREATE TABLE public.Datasets
(
  ID INTEGER NOT NULL,
  Label VARCHAR(255),
  Rowcount INTEGER,
                Description VARCHAR
  (1000),
                Varnum INTEGER,
                Effect_type VARCHAR NOT NULL,
                CONSTRAINT datasets_id_idx PRIMARY KEY
  (ID)
);


  CREATE INDEX label_datasets_idx
 ON public.Datasets
 ( Label ASC );

  CREATE TABLE public.DatasetToMetaData
  (
    ID INTEGER NOT NULL,
    DatasetMetaData_ID INTEGER NOT NULL,
    Dataset_ID INTEGER NOT NULL,
    CONSTRAINT datasettometadata_id_idx PRIMARY KEY (ID)
  );


  CREATE INDEX dataset_id_datasettometadata_idx
 ON public.DatasetToMetaData
 ( Dataset_ID ASC );

  CREATE TABLE public.Associations
  (
    ID INTEGER NOT NULL,
    Dataset_ID INTEGER NOT NULL,
    Effect DOUBLE PRECISION,
    Effect_L95 DOUBLE PRECISION,
    Effect_U95 DOUBLE PRECISION,
    N INTEGER,
    P DOUBLE PRECISION,
    P_adj DOUBLE PRECISION,
    CONSTRAINT associations_id_idx PRIMARY KEY (ID)
  );


  CREATE INDEX n_idx
 ON public.Associations
 ( N ASC );

  CREATE INDEX effect_idx
 ON public.Associations
 ( Effect ASC );

  CREATE INDEX p_adj_idx
 ON public.Associations
 ( P_adj ASC );

  CREATE INDEX dataset_id_idx
 ON public.Associations
 ( Dataset_ID ASC );

  CREATE TABLE public.StrVal
  (
    ID INTEGER NOT NULL,
    Value VARCHAR(1000) NOT NULL,
    Association_id INTEGER NOT NULL,
    Metavariable_id INTEGER NOT NULL,
    CONSTRAINT strval_id_idx PRIMARY KEY (ID)
  );


  CREATE TABLE public.NumVal
  (
    ID INTEGER NOT NULL,
    Value DOUBLE PRECISION NOT NULL,
    Association_id INTEGER NOT NULL,
    Metavariable_id INTEGER NOT NULL,
    CONSTRAINT numval_id_idx PRIMARY KEY (ID)
  );


  CREATE TABLE public.AssociationToVariable
  (
    ID INTEGER NOT NULL,
    Association_id INTEGER NOT NULL,
    Variable_id INTEGER NOT NULL,
    CONSTRAINT associationtovariable_id_idx PRIMARY KEY (ID)
  );


  CREATE INDEX associationtovariable_idx
 ON public.AssociationToVariable
 ( Variable_id ASC );

  CREATE INDEX associationtovariable_idx1
 ON public.AssociationToVariable
 ( Association_id ASC );

  ALTER TABLE public.NumVal ADD CONSTRAINT metavariable_numval_fk
FOREIGN KEY (Metavariable_id)
REFERENCES public.Metavariables (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.StrVal ADD CONSTRAINT metavariable_strval_fk
FOREIGN KEY (Metavariable_id)
REFERENCES public.Metavariables (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.AssociationToVariable ADD CONSTRAINT variables_associationtovariable_fk
FOREIGN KEY (Variable_id)
REFERENCES public.Variables (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.DatasetToMetaData ADD CONSTRAINT datasetmetadata_datasettometadata_fk
FOREIGN KEY (DatasetMetaData_ID)
REFERENCES public.DatasetMetaData (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.Associations ADD CONSTRAINT dataset_associations_fk
FOREIGN KEY (Dataset_ID)
REFERENCES public.Datasets (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.DatasetToMetaData ADD CONSTRAINT dataset_datasettometadata_fk
FOREIGN KEY (Dataset_ID)
REFERENCES public.Datasets (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.AssociationToVariable ADD CONSTRAINT associations_associationtovariable_fk
FOREIGN KEY (Association_id)
REFERENCES public.Associations (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.NumVal ADD CONSTRAINT associations_numval_fk
FOREIGN KEY (Association_id)
REFERENCES public.Associations (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;

  ALTER TABLE public.StrVal ADD CONSTRAINT associations_strval_fk
FOREIGN KEY (Association_id)
REFERENCES public.Associations (ID)
ON DELETE NO ACTION
ON UPDATE NO ACTION
  NOT DEFERRABLE;
