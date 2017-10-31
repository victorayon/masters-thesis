import datetime as dt
from time import sleep
import sys

nxttime=0

while True:
    currenttime=dt.datetime.now().strftime('%H:%M:%S')
    if currenttime == nxttime:
        pass
    else:
        open('donewritingtoo.txt','w')
        print(currenttime)
        sys.stdout.flush()
        nxttime=currenttime
        
