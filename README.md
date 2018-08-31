# RLIMS
This shiny app serves as a LIMS system for storage and management of biological specimens.

This demo version uses a SQLite database, but it is recommended to set up a server-based database management system, such as MySQL or PostgreSQL, for a productive database. SQLite databases are stored in memory or on the local machine and are accessed by one user only. You can use the pool package to manage access from multiple users.

Download sqlite3, a command line program that allows to execute SQL code from the command line ( https://sqlite.org/download.html). You will also need the RSQLite package that is available on CRAN.

The bash script createDatabase.sh is used to create the database.
