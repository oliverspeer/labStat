
CREATE TABLE IF NOT EXISTS MeasurementData (
          Tagesnummer TEXT,
          Probennummer INTEGER DEFAULT NULL,
          Fallnummer INTEGER,
          DOB DATE,
          Geschlecht TEXT,
          KundenID TEXT,
          sex INTEGER,
          Datum DATE,
          Jahr INTEGER,
          Quartal INTEGER,
          Monat INTEGER,
          Woche INTEGER,
          Tag INTEGER,
          "Alter" NUMERIC,
          Werte TEXT,
          Bezeichnung TEXT, 
          Methode INTEGER,
          WerteID INTEGER PRIMARY KEY AUTOINCREMENT,
          FOREIGN KEY(Methode) REFERENCES MethodData(Methode) ON UPDATE CASCADE,
          FOREIGN KEY(KundenID) REFERENCES CustomerData(KundenID) ON UPDATE CASCADE
          
); 