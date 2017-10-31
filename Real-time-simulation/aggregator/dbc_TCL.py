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
remote_info = {'HOST_NAME': HOST_NAME, 'HOST_USER': HOST_USER, 'HOST_PW': HOST_PW}

#Initialize a db object
db = energy_management_system.database_accessor.dbms_io.Dbms_io(tunneling=True, remote_info=remote_info, db_info=db_info)

counter=0

while True:
    #query the database for power setpoint
#    print('getting scheduled power setpoint from database')
#    try:
##        query = dbms_queries.most_recent_schedule(database_name="mri_rt",table_name="schedule_resource",condition="resource_id=9",limit_number=1)
#        query = dbms_queries.select_ag()
#        db.execute_query(query)
#        #print(db.query_results['s'])
#        db.query_results['s'].to_csv('power_setpoint.csv',index=False)
#    except:
#        print('ERROR: failed to get power value')
#        pass

    while os.path.exists('donewritingtoo.txt') == False:
        pass
    else:
# uncomment line below when ready to send TCL states to database
#        TCL_states = pd.read_csv('states.csv',names=['datetime','soc','power','capacity'])
        counter=counter+1
        print('counter=',counter)
        print('sending broadcast signal')
        try:
            #copy loads file to aggregator via ssh tunnel
            os.system("scp broadcast_ctrl.csv crpi-1@129.24.69.180:~/Documents/mu-grid/dev/mugrid_package/energy_management_system/load_generator/")
        except:
            print('ERROR: failed to send signal')
            pass

#        print('sending soc, power, & capacity to database')
#        try:
#             db.db_insert(df=TCL_states, table_name="measured_resource")
#        except:
#             print('ERROR: failed to write to database')
#             pass

        finally:
            sys.stdout.flush()
            os.remove('donewritingtoo.txt')

print('kicked out of loop ;(')

#Terminate db object
db.close_connections()
