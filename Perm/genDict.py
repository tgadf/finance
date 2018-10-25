import sys
import getopt
import json
import pprint
import argparse

def convChangedKeyVal(datfile, jsonfile):
    f = open(datfile)
    catdict = {}
    for row in f.readlines():
#        print row
        line = row.split('\n')[0]
        isID = True
        isKey = False
        for cat in line.split('\t'):
            if len(cat) > 0:
                if isID:
                    myid = cat
                    isID = False
                    isKey = True
                else:
                    if isKey:
                        key = cat
                    else:
                        value = cat
                        isID = True
                    isKey = not isKey

#        print myid, key, value
        tmpdict = {}
        tmpdict[key] = value
        #print myid, tmpdict
        catdict[myid] = tmpdict
    with open('schema/'+jsonfile, 'w') as outfile:
        json.dump(catdict, outfile)
        outfile.close()
        print 'wrote',outfile,'with',len(catdict),'entries.'

def convKeyVal(datfile, jsonfile):
    f = open(datfile)
    catdict = {}
    for row in f.readlines():
#        print row
        line = row.split('\n')[0]
        isKey = True
        for cat in line.split('\t'):
            if len(cat) > 0:
                if isKey:
                    key = cat
                else:
                    value = cat
                isKey = not isKey

        catdict[key] = value
    with open('schema/'+jsonfile, 'w') as outfile:
        json.dump(catdict, outfile)
        outfile.close()
        print 'wrote',outfile,'with',len(catdict),'entries.'

def convDat(datfile, jsonfile):
    f = open(datfile)
    categories = f.read()
    cats = categories.split('\n')
    catdict = {}
    catval = 1
    for cat in cats:
        if len(cat) > 0:
            catdict[cat] = catval
            catval += 1
    with open('schema/'+jsonfile, 'w') as outfile:
        json.dump(catdict, outfile)
        outfile.close()
        print 'wrote',outfile,'with',len(cats),'entries.'


def convHumanCorrtoCorr(humanjsonfile, jsonfile):
    data = json.load(open('schema/'+humanjsonfile))
    print 'loaded',len(data),'corr payees.'
    corrs = {}
    for key,value in data.iteritems():
        for corr in value:
            if corrs.get(corr):
                print 'duplicate!!!'
                print corr,value,key
                return
            corrs[corr] = key

    with open('schema/'+jsonfile, 'w') as outfile:
        json.dump(corrs, outfile)
        outfile.close()
        print 'wrote',outfile,'with',len(corrs),'entries.'



def convCorrtoHumanCorr(jsonfile, payeefile, humanjsonfile):
    data = json.load(open('schema/'+jsonfile))
    payees = json.load(open('schema/'+payeefile))
    print 'loaded',len(payees),'payees.'
    
    corrs = {}
    for payee in payees.keys():
        corrs[payee] = []

    for key,value in data.iteritems():
        if value in corrs:
            corrs[value].append(key)
        else:
            print value," is not in the payee list..."

    with open('schema/'+humanjsonfile, 'w') as outfile:
        json.dump(corrs, outfile)
        outfile.close()
        print 'wrote',outfile,'with',len(corrs),'entries.'




if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--output')
    parser.add_argument('-c', '--category', dest='makecat', action='store_true')
    parser.add_argument('-v', dest='verbose', action='store_true')
    args = parser.parse_args()
    
    convCorrtoHumanCorr("correctedPayeeNames.json", "payees.json", "correctedPayeeNames.human.json")
    convHumanCorrtoCorr("correctedPayeeNames.human.json", "correctedPayeeNames.comp.json")
#    convDat("Categories.dat", "categories.json")
#    convDat("Payees.dat", "payees.json")
#    convKeyVal("CategoryCodeForPayees.dat", "categoryCodeForPayees.json")
#    convChangedKeyVal("ChangedPayeeCategory.dat", "changedPayeeCategory.json")
#    convKeyVal("CorrectedPayeeNames.dat", "correctedPayeeNames.json")
