
CREATE TABLE TarifData (
           TarifID	INTEGER	PRIMARY KEY AUTOINCREMENT,
           Methode	INTEGER,
           Bezeichnung	TEXT,
           Jahr	INTEGER,
           EAL_Position	NUMERIC,
           Txp	NUMERIC,
           FOREIGN KEY(Methode) REFERENCES MethodData(Methode) ON UPDATE CASCADE,
           FOREIGN KEY(EAL_Position) REFERENCES EALData(EAL_Position) ON UPDATE CASCADE
          ) 