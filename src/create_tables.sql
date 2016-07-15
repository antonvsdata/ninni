
CREATE SEQUENCE public.variables_id_seq;

CREATE TABLE public.Variables (
                ID INTEGER NOT NULL DEFAULT nextval('public.variables_id_seq'),
                Label VARCHAR(255),
                Description VARCHAR(1000),
                CONSTRAINT variables_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.variables_id_seq OWNED BY public.Variables.ID;

CREATE SEQUENCE public.datasetmetadata_id_seq;

CREATE TABLE public.DatasetMetaData (
                ID INTEGER NOT NULL DEFAULT nextval('public.datasetmetadata_id_seq'),
                Label VARCHAR(255),
                Description VARCHAR(1000),
                CONSTRAINT datasetmetadata_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.datasetmetadata_id_seq OWNED BY public.DatasetMetaData.ID;

CREATE SEQUENCE public.datasets_id_seq;

CREATE TABLE public.Datasets (
                ID INTEGER NOT NULL DEFAULT nextval('public.datasets_id_seq'),
                Label VARCHAR(255),
                Description VARCHAR(1000),
                Varnum INTEGER,
                Effect_type VARCHAR NOT NULL,
                CONSTRAINT datasets_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.datasets_id_seq OWNED BY public.Datasets.ID;

CREATE SEQUENCE public.datasettometadata_id_seq;

CREATE TABLE public.DatasetToMetaData (
                ID INTEGER NOT NULL DEFAULT nextval('public.datasettometadata_id_seq'),
                Dataset_ID INTEGER NOT NULL,
                DatasetMetaData_ID INTEGER NOT NULL,
                CONSTRAINT datasettometadata_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.datasettometadata_id_seq OWNED BY public.DatasetToMetaData.ID;

CREATE SEQUENCE public.associations_id_seq;

CREATE TABLE public.Associations (
                ID INTEGER NOT NULL DEFAULT nextval('public.associations_id_seq'),
                Dataset_ID INTEGER NOT NULL,
                Effect DOUBLE PRECISION,
                Effect_L95 DOUBLE PRECISION,
                Effect_U95 DOUBLE PRECISION,
                N INTEGER,
                P DOUBLE PRECISION,
                P_FDR DOUBLE PRECISION,
                CONSTRAINT associations_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.associations_id_seq OWNED BY public.Associations.ID;

CREATE INDEX n_idx
 ON public.Associations
 ( N ASC );

CREATE INDEX p_idx
 ON public.Associations
 ( P ASC );

CREATE INDEX effect_idx
 ON public.Associations
 ( Effect ASC );

CREATE INDEX p_fdr_idx
 ON public.Associations
 ( P_FDR ASC );

CREATE SEQUENCE public.associationtovariable_id_seq;

CREATE TABLE public.AssociationToVariable (
                ID INTEGER NOT NULL DEFAULT nextval('public.associationtovariable_id_seq'),
                Association_id INTEGER NOT NULL,
                Variable_id INTEGER NOT NULL,
                CONSTRAINT associationtovariable_id_idx PRIMARY KEY (ID)
);


ALTER SEQUENCE public.associationtovariable_id_seq OWNED BY public.AssociationToVariable.ID;

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
