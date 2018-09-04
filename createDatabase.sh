#!/bin/bash

prj=$(sqlite3 rlims.db "CREATE TABLE project
(
	prjid INTEGER PRIMARY KEY,
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
);")


usr=$(sqlite3 rlims.db "CREATE TABLE users
(
	usrid INTEGER PRIMARY KEY,
	usrinitials VARCHAR(4) NOT NULL,
	usrfirstname VARCHAR(45),
	usrlastname VARCHAR(45),
	usrposition VARCHAR(45),
	usrstartdate VARCHAR(10),
	usrenddate VARCHAR(10)
);")


pat=$(sqlite3 rlims.db "CREATE TABLE patient
(
	patid INTEGER PRIMARY KEY,
	patpatientid VARCHAR(5) NOT NULL,
	patpseudoid VARCHAR(20),
	patprjidref INT REFERENCES project(prjid) ON DELETE RESTRICT ON UPDATE CASCADE,
	patdiagnosis VARCHAR(200),
	patdiagnosissub VARCHAR(200),
	patsex VARCHAR(4),
	patcomment VARCHAR(200)
);")


smp=$(sqlite3 rlims.db "CREATE TABLE sample
(
	smpid INTEGER PRIMARY KEY,
	smppatidref INT REFERENCES patient(patID) ON DELETE CASCADE ON UPDATE CASCADE,
	smpsampleid VARCHAR(45) NOT NULL,
	smpsampledate VARCHAR(10),
	smpdatereceived VARCHAR(10),
	smpleukocytes INT,
	smppblymphocytes INT,
	smpcomment VARCHAR(100)
);")


anu=$(sqlite3 rlims.db "CREATE TABLE analysislookup
(
	anuid INTEGER PRIMARY KEY,
	anuname VARCHAR(20),
	anucat VARCHAR(40),
	anudescription text
);")


anl=$(sqlite3 rlims.db "CREATE TABLE analysis
(
	anlid INTEGER PRIMARY KEY,
	anlsmpidref INT REFERENCES sample(smpid) ON DELETE CASCADE ON UPDATE CASCADE,
	anlanuidref INT REFERENCES analysislookup(anuid) ON DELETE RESTRICT ON UPDATE CASCADE,
	anlstatus VARCHAR(40),
	anldate VARCHAR(10),
	anlrun VARCHAR(40),
	anltype VARCHAR(20),
	anlcomment VARCHAR(100)
);")


alq=$(sqlite3 rlims.db "CREATE TABLE aliquot
(
	alqid INTEGER PRIMARY KEY,
	alqsmpidref INT REFERENCES sample(smpid) ON UPDATE CASCADE ON DELETE CASCADE,
	alqdate VARCHAR(10),
	alqusridref INT REFERENCES users(usrid) ON UPDATE CASCADE ON DELETE RESTRICT,
	alqsampletype VARCHAR(40),
	alqcelltype VARCHAR(40),
	alqcellnumber BIGINT,
	alqvolume INT,
	alqconc REAL,
  alqfreezer VARCHAR(20),
  alqtower INT,
  alqbox INT,
  alqposition VARCHAR(20),
	alqempty BOOLEAN,
	alqdateused VARCHAR(10),
	alqusedusridref INT REFERENCES users(usrid) ON UPDATE CASCADE ON DELETE RESTRICT,
	alqpurpose VARCHAR(40),
	alqcomment VARCHAR(200)
);")
