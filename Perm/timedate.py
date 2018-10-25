from datetime import datetime


##########################################
#
# Get/Parse date
#
##########################################
def makeDateFromDT(date):
    mon  = date.month
    day  = date.day
    year = date.year
    date = str(mon)+"/"+str(day)+"/"+str(year)
    return date


def parseDate(date):
    if date.count("-") == 1:
        year,mon=date.split("-")
        return [int(mon), 1, year]
    elif date.count("-") == 2:
        year,mon,day=date.split("-")
        return [int(mon), int(day), year]
    elif date.count("/") == 2:
        mon,day,year=date.split("/")
        year=int(year)
        if year < 2000:
            year += 2000
        return [int(mon), int(day), year]
    elif date.count("/") == 1:
        mon,year=date.split("/")
        year=int(year)
        if year < 2000:
            year += 2000
        return [int(mon), 1, year]
    elif date.count("/") == 0:        
        year=int(year)
        if year < 2000:
            year += 2000
        return [1, 1, year]
    else:
        print "Could not parse",date
        exit()


def getdtime(date):
    mon,day,year = parseDate(date)
    dtime        = datetime( year, mon, day )
    return dtime


def getYearMonth(date):
    d = getdtime(date)
    retval = d.strftime("%Y-%m")
    return retval


def getDMY(date):
    d = getdtime(date)
    retval = d.strftime("%d/%m/%Y")
    return retval


def getDate(date):
    vals = None
    if date.count("/") == 2: vals=date.split('/')
    if date.count("-") == 2: vals=date.split('-')
    try:
        vals=[int(x) for x in vals]
    except:
        print "Can not parse date",date
        exit()

    # Check for year/mon/day -> mon/day/year
    if vals[0] > 1000:
        newvals=[vals[1], vals[2], vals[0]]
        newvals=[str(x) for x in newvals]
        return "/".join(newvals)
    elif vals[2] > 1000:
        return date
    elif vals[0] < 1000 and vals[1] < 1000 and vals[2] < 1000:
        newvals=[vals[0], vals[1], 2000+vals[2]]
        newvals=[str(x) for x in newvals]
        return "/".join(newvals)
    else:
        print "Not sure how to parse date", date
        exit()



def getYear(idate):
    return idate.year


def getDateText(startdate, enddate):
    start=[]
    start.append(startdate.year)
    start.append(startdate.month)
    start.append(startdate.day)
    end=[]
    if startdate.year == enddate.year:
        if startdate.month == enddate.month:
            end.append(enddate.day)
        else:
            end.append(enddate.month)
            end.append(enddate.day)
    else:
        end.append(enddate.year)
        end.append(enddate.month)
        end.append(enddate.day)
        
    start=[str(x) for x in start]
    startval=".".join(start)
    end=[str(x) for x in end]
    endval=".".join(end)
    return startval+"--"+endval


def convDate(dval):
    vals=dval.split(',')
    try:
        year=int(vals[1].strip())
    except:
        print "Problem parsing year from",vals
        exit()

    try:
        month,day=vals[0].split()
    except:
        print "Problem parsing month,day from",vals
        exit()

    mvals={}
    mvals["January"]="01"
    mvals["February"]="02"
    mvals["March"]="03"
    mvals["April"]="04"
    mvals["May"]="05"
    mvals["June"]="06"
    mvals["July"]="07"
    mvals["August"]="08"
    mvals["September"]="09"
    mvals["October"]="10"
    mvals["November"]="11"
    mvals["December"]="12"
    for mval,mdval in mvals.iteritems():
        month=month.replace(mval, mdval)

    date=month+"/"+day+"/"+str(year)
    return date




def getTimeline(data):
    start=None
    end=None
    timedata=[]
    if isinstance(data, list):
        timedata.append(data)
    elif isinstance(data, dict):
        tdata=[]
        for k,v in data.iteritems():
            tdata.append(v)
        timedata.append(tdata)

    for tdata in timedata:
        for val in tdata:
            date=val['date']
            mon,day,year=parseDate(date)
            if year < 2000:
                print date
                raise()
            dtime=datetime( year, mon, day )
            if start == None or end == None:
                start = dtime
                end = dtime
            else:
                if dtime < start:
                    start = dtime
                if dtime > end:
                    end = dtime

    if start == None or end == None:
        print "No timeline information in data!"
        print len(data)
        raise()
    return start,end
