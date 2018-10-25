import sys
import getopt
import json
import pprint
import argparse
import os

def nice(val, maxlen):
    sval=str(val)
    while len(sval) < maxlen:
        sval = " "+sval
    return sval

def show(val, maxlen):
    size=len(str(val))
    extra=maxlen-size
    name=str(val)
    tmp=""
    for i in range(size-1):
        tmp=' '.join(tmp)
        print tmp,len(tmp)
        
    return name+tmp


def getIDCase(test, data):
    if isinstance(data, list):
        for key in data:
            if test.upper() == key.upper():
                return key, True
            if test == key:
                return key, True
        return "X",False
    
    if isinstance(data, dict):
        for key in data:
            if test.upper() == key.upper():
                return key,True
            if test in key:
                return key,True
        return "X",False


def getID(key, data):
    if isinstance(data, list):
        if key in data:
            return key, True
        else:
            return "X",False
    if isinstance(data, dict):
        if data.get(key) == None:
            #        print "No ID for",key
            return "X",False
        else:
            return key,True

def getKeysFromValue(data, value):
    retvals={}
    for k,v in data.iteritems():
        if v.find(value) != -1:
            if retvals.get(v) == None:
                retvals[v]=[]
            retvals[v].append(k)
    return retvals


########################################################################################################
#
#
# Payee Corrections
#
#
########################################################################################################
def getCorrPayee(payee):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/correctedPayeeNames.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        for key,corrpayee in data.iteritems():
            #print key,payee,corrpayee
            if payee.find(key) != -1:
                return corrpayee
    return payee


def mvCorrPayee(payeeFrom, payeeTo):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/correctedPayeeNames.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        payeeid,found=getPayeeID(payeeTo, True)
        if not found:
            print "Payee[",payeeTo,"] is not known."
            exit()
        keys=data.keys()
        nfix=0
        for k in keys:
            if data[k] == payeeFrom:
                nfix += 1
                print '[',k,'] ->',payeeFrom,' ---> ',payeeTo
                data[k] = payeeTo
        print "Fixed",nfix,"entries."
        json.dump(data, open(jsonfile, 'w'))
        
        return True
    return False

def addCorrPayee(originalpayee, correctedpayee):
    payee,found=getPayee(correctedpayee, search=False)
    if found:
        fdir=os.path.dirname(os.path.realpath(__file__))
        jsonfile=fdir+"/schema/correctedPayeeNames.json"
        with open(jsonfile, 'r') as infile:
            data = json.load(infile)
            infile.close()
            data[originalpayee] = payee
            jsonfile=fdir+"/schema/correctedPayeeNames.json"
            json.dump(data, open(jsonfile, 'w'))
            print 'Mapping [',originalpayee,' --> ',payee,']'
            return True
    else:
        print 'Payee[',payee,'] does not have an ID so I could not add a correction.'

    return False


def showCorrPayee(payee):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/correctedPayeeNames.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        keys = getKeysFromValue(data, payee)
        return keys

    return None


def getCorrPayees():
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/correctedPayeeNames.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        retvals={}
        for k,v in data.iteritems():
            if retvals.get(v) == None:
                retvals[v]=[]
            retvals[v].append(k)
        return retvals

def rmCorrPayee(corr):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/correctedPayeeNames.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if data.get(corr):
            del data[corr]
            jsonfile=fdir+"/schema/correctedPayeeNames.json"
            json.dump(data, open(jsonfile, 'w'))
            print 'Removed',corr,'from',jsonfile
            return True
        else:
            print 'Could not remove',corr,'from',jsonfile,'because it does not exist.'

    return False


########################################################################################################
#
#
# Category
#
#
########################################################################################################
def isKnownCategory(category):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/indivcatmap.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        if data.get(category) == None:
            return False
        return True



########################################################################################################
#
#
# Payee/Category
#
#
########################################################################################################
def getPayeeCategory(payee):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categoryCodeForPayees.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()        
        if data.get(payee):
            return data[payee],True
        else:
            return "X",False
    return "X",False


def showPayeeCategories():
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categoryCodeForPayees.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        payees=sorted(data.keys())
        for payee in payees:
            print nice(payee, 30),nice(data[payee], 30)


def getPayeeCategories():
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categoryCodeForPayees.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        payees=sorted(data.keys())
        retval=[]
        for payee in payees:
            retval.append([str(payee),str(data[payee])])
        return retval


def addPayeeCategory(payee, category):
    payeeid,found=getPayeeID(payee, True)
    if found:
        fdir=os.path.dirname(os.path.realpath(__file__))
        jsonfile=fdir+"/schema/categoryCodeForPayees.json"
        data=json.load(open(jsonfile, 'r'))
        if getCategoryID(category) == -1:
            print 'Category',category,'does not exist. You must create it first.'
            return False
        data[payee] = category
        json.dump(data, open(jsonfile, 'w'))
        print 'Added',payee,' -> ',category,'to',jsonfile
        return True
    else:
        print 'Payee',payee,'does not exist.'
        return False



def rmPayee(payee):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if data.get(payee):
            print 'Removed',payee,'from',jsonfile
            del data[payee]
            save=True
        else:
            save=False
        if save:
            jsonfile=fdir+"/schema/payee.json"
            json.dump(data, open(jsonfile, 'w'))


    jsonfile=fdir+"/schema/categoryCodeForPayees.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if data.get(payee):
            print 'Removed',payee,'from',jsonfile
            del data[payee]
            save=True
        else:
            save=False
        if save:
            jsonfile=fdir+"/schema/categoryCodeForPayees.json"
            json.dump(data, open(jsonfile, 'w'))
    return True
        
    return False

def addPayee(payee):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if data.get(payee):
            print payee,"already exists."
            return True

        data[payee] = 1
        jsonfile=fdir+"/schema/payee.json"
        json.dump(data, open(jsonfile, 'w'))
        print 'Added',payee,'to',jsonfile
        return True

    return False

def getPayees():
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        return data.keys()
    return []

def getPayee(pid, search=False):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if search:
            retval=getIDCase(pid, data)
            return retval
        else:
            retval=getID(pid, data)
            return retval
    return "..."



def getPayeeID(payee, search=False):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if search == False:
            result,found=getID(payee, data)
            return result,found
        else:
            #print 'Searching for',payee
            fullresult,found=getIDCase(payee, data)
            #print 'Result',fullresult
            return fullresult,found


def isKnownPayee(payee):
    payeeid,found=getPayeeID(payee)
    if not found:
        return False
    return True


def getCategoryID(category, search=False):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categories.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        if search == False:
            result,found=getID(category, data)
            return result
        else:
            fullresult,found=getIDCase(category, data)
            return fullresult

def showCategories():
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categories.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        ncats=0
        for k, v in data.iteritems():
            print v,'\t',k,'\t',
            ncats += 1
            if ncats % 4 == 0:
                print ""

def getCategory(pid, search=False):
    fdir=os.path.dirname(os.path.realpath(__file__))
    jsonfile=fdir+"/schema/categories.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        infile.close()
        for k, v in data.iteritems():
            if v == pid:
                return k
    return "..."

def checkPayeeCategory():
    jsonfile="schema/payee.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        for key in data:
            if data[key] != getPayeeID(key):
                print "Payee",key,"has trouble."
        print "Done checking",len(data),"payee."
        infile.close()

    jsonfile="schema/categories.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        for key in data:
            if data[key] != getCategoryID(key):
                print "Category",key,"has trouble."
        print "Done checking",len(data),"categories."

    jsonfile="schema/categoryCodeForPayees.json"
    with open(jsonfile, 'r') as infile:
        data = json.load(infile)
        for key in data:
            value = data[key]
            payeeid,found = getPayeeID(key)
            catid = getCategoryID(value)
            if payeeid == -1:
                print "Payee",key,"has no ID."
            if catid == -1:
                print "Category",value,"has no ID."
        print "Done checking",len(data),"categories."

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    args = parser.parse_args()


    print getPayeeID("Fermilab")
    print getCategoryID("Check")
    print getPayeeCategory("Fermilab")
    checkPayeeCategory()
