#!/usr/bin/python
# -*- coding: utf-8 -*-

import urllib
import xml.etree.ElementTree as ET
import sys
import csv
import unicodedata
import string

from math import sin, cos, sqrt, atan2, radians

HORARI = 3
LATITUD = 4
LONGITUD = 5
PARADA = 6

ESTACIONS = './Documents/ESTACIONS_BUS.csv'
TRANSPORT = './Documents/TRANSPORTS.csv'

def distance(lat1,lon1,lat2,lon2):
    #radi aproximat de la terra:
    R = 6373.0
    lat1 = radians(lat1)
    lon1 = radians(lon1)
    lat2 = radians(lat2)
    lon2 = radians(lon2)
    dlon = lon2-lon1
    dlat = lat2-lat1

    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))

    return R*c

def remove_accents(data):
    return ''.join(x for x in unicodedata.normalize('NFKD', data) if x in string.ascii_letters).lower()
class Esdeveniments:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource).find('body').find('resultat')

#donat un nom, busquem els noms d'actes, llocs o barris que coincideixen amb el nom retorna un set amb els actes
    def busca(self,nom):
        llista = []
        for acte in self.root.find('actes').findall('acte'):

            nomActe = acte.find('nom').text.encode('utf8')
            nomActe = remove_accents(nomActe.decode('utf8'))

            nomLloc = acte.find('lloc_simple').find('nom').text.encode('utf8')
            nomLloc = remove_accents(nomLloc.decode('utf8'))
            nomBarri = acte.find('lloc_simple').find('adreca_simple').find('barri').text
            if nomBarri != None:
                nomBarri = remove_accents(nomBarri.encode('utf8').decode('utf8'))

            try:
                if (nom in nomActe or nom in nomLloc or nom in nomBarri):
                    llista.append(acte)
            except TypeError:
                if (nom in nomActe or nom in nomLloc):
                    llista.append(acte)
        return llista

    #funcio d'ordenacio de les estacions
def getDist(item):
    return item[0]

class Bicing:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource)

    #donada una posicio, retorna una llista de les n estacions mes properes
    def getStations(self,position,n):
        list = []
        list2 = []
        for station in self.root.findall('station'):
            #agafem la latitud i longitud
            lat = float(station.find('lat').text)
            lon = float(station.find('long').text)
            dist = distance(position[0],position[1],lat,lon)
            freeSlots = int(station.find('slots').text)
            bicis = int(station.find('bikes').text)
            if dist < 0.5 and freeSlots>0: list.append([dist,station])
            if dist <0.5 and bicis>0 : list2.append([dist,station])
        list.sort(key=getDist)
        list2.sort(key=getDist)
        return [list[:n],list2[:n]]

class Busos:
    def __init__(self,csvSource):
        reader = csv.reader(csvSource, delimiter='\t')
        self.rows =[]
        for elem in reader:
            row = elem[0].split(';')
            if row[0] != 'CODI_CAPA':
                self.rows.append(row)


    #donada una posicio, retorna una llista de les n estacions mes properes
    def getStations(self,position,n):
        list = []
        for row in self.rows:
            lat = float(row[LATITUD])
            lon = float(row[LONGITUD])
            dist = distance(position[0],position[1],lat,lon)
            if dist < 0.5: list.append([dist,row])
        return sorted(list,key=getDist)
class Transports:
    def __init__(self,csvSource):
        reader = csv.reader(csvSource, delimiter='\t')
        self.rows =[]
        for elem in reader:
            row = elem[0].split(';')
            if row[0] != 'CODI_CAPA':
                self.rows.append(row)
    #donada una posicio, retorna una llista de les n estacions mes properes
    def getStations(self,position,n):
        list = []
        for row in self.rows:
            lat = float(row[LATITUD])
            lon = float(row[LONGITUD])
            dist = distance(position[0],position[1],lat,lon)
            if dist < 0.5: list.append([dist,row])
        return sorted(list,key=getDist)

def getActes(elem):
    elem = remove_accents(elem.decode('utf8'))
    actes = esdeveniments.busca(elem)
    print 'searching for',elem,'got',len(actes),'results'
    return actes

def cerca(lista):
    if isinstance(lista,str): return getActes(lista)
    if isinstance(lista,tuple):
        ret = []
        for elem in lista:
            result = cerca(elem)
            ret = set(ret) | set(result)
        return ret
    if isinstance(lista,list):
        primer = True
        ret = []
        for elem in lista:
            if primer :
                ret = cerca(elem)
                primer = False
            else :
                result = cerca(elem)
                ret = set(ret) & set(result)
        return ret

#output en taula html
def initTable():
    file = open('dataQuery.dat','w+')
    file.write('<!DOCTYPE html> <html> <head> <style> table, th, td { border: 1px solid black;'+
               'border-collapse: collapse;} th, td { padding: 15px;} </style>'+
               '</head><body> <H3>Resultado de la busqueda:'+
               sys.argv[1]+' '+sys.argv[2]+'</H3><br></body></html>')
    #taula
    file.write('<table style ="width:70%">')
    file.write('<tr><th>Acte</th><th>adreca</th><th>data</th><th>transport</th></tr>')
    return file


def addToTableTrans(acte,medi,file):
    nomActe = acte.find('nom').text
    adrecaActe = (acte.find('lloc_simple').find('adreca_simple').find('carrer').text + ' Numero: '+
                    acte.find('lloc_simple').find('adreca_simple').find('numero').text)
    dataActe = acte.find('data').find('data_proper_acte').text
    file.write('<tr>')
    file.write('<td>'+nomActe.encode('utf8')+'</td>')
    file.write('<td>'+adrecaActe.encode('utf8')+'</td>')
    file.write('<td>'+dataActe.encode('utf8')+'</td>')
    trans =''
    for elem in medi:
        trans = trans + elem[PARADA]+','
    file.write('<td>'+trans[:-1]+'</td>')
    file.write('</tr>')

def addToTableBici(acte,medi,file):
    nomActe = acte.find('nom').text
    adrecaActe = (acte.find('lloc_simple').find('adreca_simple').find('carrer').text + ' Numero: '+
                    acte.find('lloc_simple').find('adreca_simple').find('numero').text)
    dataActe = acte.find('data').find('data_proper_acte').text
    file.write('<tr>')
    file.write('<td>'+nomActe.encode('utf8')+'</td>')
    file.write('<td>'+adrecaActe.encode('utf8')+'</td>')
    file.write('<td>'+dataActe.encode('utf8')+'</td>')
    trans ='Estacions amb llocs lliures: '
    for elem in medi[0]:
        trans = trans + elem[1].find('id').text+','
    trans2 = 'Estacions amb bicis lliures: '
    for elem in medi[1]:
        trans2 = trans2 + elem[1].find('id').text+','
    file.write('<td>'+trans[:-1]+'<br>'+trans2[:-1]+'</td>')
    file.write('</tr>')

def getEstacionsProperesBici(acte):
    for coord in acte.iter('googleMaps'):
        lon = -8000
        lat = -8000
        if coord.get('lon') != " ":
            lon = float(coord.get('lon'))
            lat = float(coord.get('lat'))
        ret = bicing.getStations([lat,lon],5)
        print 'Posibles estacions de bicing:',len(ret[0])+len(ret[1])
        return ret

def getEstacionsProperesTransport(acte):
    l = []
    for coord in acte.iter('googleMaps'):
        lon = -8000
        lat = -8000
        if coord.get('lon') != " ":
            lon = float(coord.get('lon'))
            lat = float(coord.get('lat'))
        ret = busos.getStations([lat,lon],6)
        print 'Posibles estacions de Bus:',len(ret)
        if len(ret) != 0: l.append(ret)
        ret = transports.getStations([lat,lon],6)
        print 'Posibles estacions de Tren:',len(ret)
        if len(ret) != 0: l.append(ret)
    return l

def donaTransport(actes,opcions):
    for acte in actes:
        print 'buscant transport mes adecuat per l\'acte',acte.find('nom').text
        for i in opcions:
            if i == 'bicing':
                listA = getEstacionsProperesBici(acte)
                addToTableBici(acte,listA,file)
                break
            if ('transport' in i):
                bm = getEstacionsProperesTransport(acte)
                if bm != []:
                    day = False
                    night = False
                    list = []
                    for elem in bm[0]:
                        if 'Day' in elem[1][HORARI] and not day:
                            list.append(elem[1])
                            day = True
                        if 'Night' in elem[1][HORARI] and not night:
                            list.append(elem[1])
                            night = True
                        if day and night : break

                    for elem in bm[1]:
                        if not any(elem[1][0] in s for s in list):
                            list.append(elem[1])

                    for elem in bm[0]:
                        if not any(elem[1][0] in s for s in list):
                            list.append(elem[1])

                    addToTableTrans(acte,list,file)
                    break
            else:
                break


sock = urllib.urlopen("http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199")
xmlSource = sock.read()
sock.close()
esdeveniments = Esdeveniments(xmlSource)

#agafem l'entrada y la dividim en conjuncions
lista = eval(sys.argv[1])

actes = cerca(lista)
print 'got',len(actes),'results'


sock = urllib.urlopen("http://wservice.viabicing.cat/getstations.php?v=1")
xmlSource = sock.read()
sock.close()
bicing = Bicing(xmlSource)

ifile  = open(ESTACIONS, "r")
busos = Busos(ifile)
ifile2 = open(TRANSPORT,"r")
transports = Transports(ifile2)
file = initTable()

#parseig medi transport
opcions = eval(sys.argv[2])
donaTransport(actes,opcions)
ifile.close()


print 'all done, output: dataQuery.dat'
