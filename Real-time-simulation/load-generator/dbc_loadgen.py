from datetime import datetime, timedelta
import requests
import pymysql
import energy_management_system.database_accessor.dbms_queries as dbms_queries
import energy_management_system.database_accessor.dbms_io
import pandas as pd
import os
import sys

#user info
HOST_NAME = "129.24.68.15"
HOST_USER = "vpp-study"
HOST_PW = "12UnM3snL58"

#db info
DB_HOST = "localhost"
DB_PORT = 3306
DB_USER = "residential_generator"
DB_PW = "20rez_gen17"
DB_NAME = "mri_rt"
db_info = {'DB_HOST' : DB_HOST, 'DB_PORT' : DB_PORT, 'DB_USER' : DB_USER, 'DB_PW' : DB_PW, 'DB_NAME' : DB_NAME}
dbh_info = {'DB_HOST' : DB_HOST, 'DB_PORT' : DB_PORT, 'DB_USER' : DB_USER, 'DB_PW' : DB_PW, 'DB_NAME' : "mri_historian"}
remote_info = {'HOST_NAME': HOST_NAME, 'HOST_USER': HOST_USER, 'HOST_PW': HOST_PW}

#Initialize a db object
db = energy_management_system.database_accessor.dbms_io.Dbms_io(tunneling=True, remote_info=remote_info, db_info=db_info)
db_hist = energy_management_system.database_accessor.dbms_io.Dbms_io(tunneling=True, remote_info=remote_info, db_info=dbh_info)

while True:
    while os.path.exists('donewriting.txt') == False:
        pass
    else:
        #read transformer loads file
        loads = pd.read_csv('transformers.csv',names=['load_id','datetime','measured_value'])
#        print(loads.head(1))
        print('sending TCL_status.csv')
        try:
            #copy loads file to aggregator via ssh tunnel
            os.system("scp TCL_status.csv crpi-2@129.24.68.154:~/Documents/mu-grid/dev/mugrid_package/energy_management_system/aggregator/")
        except:
            print('ERROR: failed to send TCL_status.csv to aggregator')
            pass

        print('sending loads to database')
        try:
            #delete from the mri_rt db
            delete_query=dbms_queries.clear_table_with_condition('mri_rt','measured_load','load_id>1')
            db.delete_query(delete_query)
            #write total residential load to database
            db.db_insert(df = loads,table_name="measured_load")
            db_hist.db_insert(df = loads,table_name="measured_load")
        except:
            print('ERROR: failed to write to database')
            pass

        print('getting current temperature from database')
        try:
            query = dbms_queries.most_recent_record("mri_rt","lg")
            db.execute_query(query)
#            print(db.query_results['t'])
            db.query_results['t'].to_csv('outside_temp.csv',index=False)
        except:
            print('ERROR: failed to get temperature value')
            pass

        finally:
            sys.stdout.flush()
            os.remove('donewriting.txt')

print('kicked out of loop ;(')

#Terminate db object
db.close_connections()

