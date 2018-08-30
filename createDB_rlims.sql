
-- Diagnosis
CREATE TABLE diagnosis
(
	dgnid INT PRIMARY KEY,
	dgnname VARCHAR(30) NOT NULL,
	dgnfullname VARCHAR(100)
);

-- Project
CREATE TABLE project
(
	prjid INT PRIMARY KEY,
	prjname VARCHAR(20) NOT NULL,
	prjdisease VARCHAR(100),
	prjmaterial VARCHAR(45),
	prjfirstname VARCHAR(45),
	prjlastname VARCHAR(45),
	prjdepartment VARCHAR(100),
	prjinstitute VARCHAR (100),
	prjcity VARCHAR(45),
	prjcountry VARCHAR(45),
	prjdescription VARCHAR(100)
);

-- User
CREATE TABLE users
(
	usrid INT PRIMARY KEY,
	usrinitials VARCHAR(4) NOT NULL,
	usrfirstname VARCHAR(45),
	usrlastname VARCHAR(45),
	usrposition VARCHAR(45),
	usrstartdate VARCHAR(10),
	usrenddate VARCHAR(10)
);

-- Patients
CREATE TABLE patient
(
	patid INT PRIMARY KEY,
	patpatientid VARCHAR(5) NOT NULL,
	patpseudoid VARCHAR(20),
	patprjidref INT REFERENCES project(prjid) ON DELETE RESTRICT ON UPDATE CASCADE,
	patdgnidref INT REFERENCES diagnosis(dgnid) ON DELETE RESTRICT ON UPDATE CASCADE,
	patsex VARCHAR(4),
	patcomment VARCHAR(200)
);

-- Sample
CREATE TABLE sample
(
	smpid INT PRIMARY KEY,
	smppatidref INT REFERENCES patient(patID) ON DELETE CASCADE ON UPDATE CASCADE,
	smpsampleid VARCHAR(45) NOT NULL,
	smpsampledate VARCHAR(10),
	smpdatereceived VARCHAR(10),
	smpleukocytes INT,
	smppblymphocytes INT,
	smpcomment VARCHAR(100)
);

-- Analysis types
CREATE TABLE analysislookup
(
	anuid INT PRIMARY KEY,
	anuname VARCHAR(20),
	anudescription text
);

-- Analysis
CREATE TABLE analysis
(
	anlid INT PRIMARY KEY,
	anlsmpidref INT REFERENCES sample(smpid) ON DELETE CASCADE ON UPDATE CASCADE,
	anlanuidref INT REFERENCES analysislookup(anuid) ON DELETE RESTRICT ON UPDATE CASCADE,
	anlstatus VARCHAR(40),
	anldate VARCHAR(10),
	anlrun VARCHAR(40),
	anltype VARCHAR(20),
	anlcomment VARCHAR(100)
);

-- Aliquot
CREATE TABLE aliquot
(
	alqid INT PRIMARY KEY,
	alqsmpidref INT REFERENCES sample(smpid) ON UPDATE CASCADE ON DELETE CASCADE,
	alqdate VARCHAR(10),
	alqusridref INT REFERENCES users(usrid) ON UPDATE CASCADE ON DELETE RESTRICT,
	alqsampletype VARCHAR(40),
	alqcelltype VARCHAR(40),
	alqcellnumber BIGINT,
	alqvolume INT,
	alqconc REAL,
	alqstored VARCHAR(20),
  alqfreezer VARCHAR(20),
  alqtower INT,
  alqbox INT,
  alqposition VARCHAR(20),
	alqempty BOOLEAN,
	alqdateused VARCHAR(10),
	alqusedusridref INT REFERENCES users(usrid) ON UPDATE CASCADE ON DELETE RESTRICT,
	alqpurpose VARCHAR(40),
	alqcomment VARCHAR(200)
);
