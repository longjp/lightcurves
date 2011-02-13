import sqlite3
import create_database
import derive_features
import noisification
import db_output
from time import time

## what columns for features in features table
features_file = "derived_features_list.txt" # where we define features



sql_cmd = '''SELECT source_id FROM sources WHERE survey='debosscher' AND number_points > 200 AND original_source_id=source_id'''

## setup the db
connection = sqlite3.connect('astronomy.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=False)
connection.commit()

