#!/usr/bin/python
# -*- coding: utf-8 -*-

import urllib
import xml.etree.ElementTree as ET
import sys
from math import sin, cos, sqrt, atan2, radians


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

class Esdeveniments:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource).find('body').find('resultat')

#donat un nom, busquem els noms d'actes, llocs o barris que coincideixen amb el nom retorna un set amb els actes
    def busca(self,nom):
        llista = []
        for acte in self.root.find('actes').findall('acte'):
            nomActe = acte.find('nom').text
            nomLloc = acte.find('lloc_simple').find('nom').text
            nomBarri = acte.find('lloc_simple').find('adreca_simple').find('barri').text
            if (nom in nomActe or nom in nomLloc or nom in nomBarri):
                llista.append(acte)
        return llista

class Bicing:
    def __init__(self,xmlSource):
        self.root = ET.fromstringlist(xmlSource)

    #donada una posicio, retorna una llista de les n estacions mes properes
    def getStations(self,position,n):
        list = []
        for station in self.root.findall('station'):
            #agafem la latitud i longitud
            lat = float(station.find('lat').text)
            lon = float(station.find('long').text)
            dist = distance(position[0],position[1],lat,lon)
            freeSlots = int(station.find('slots').text)
            if dist < 0.500 and freeSlots>0: list.append(station)
        return list[:5]


#agafa el nom sense les cometes simples
def getNom(string):
        aux = string[1:]
        aux = aux[:-1]
        return aux

#parseja l'entrada y retorna una llista de llistes pos 0 conjuncions pos 1 disyuncions
def parseja(entrada):
    disyunciones = []
    conjunciones = []
    conjuncions = entrada.split('[')
    for disyuncion in conjuncions:
        try:
            if disyuncion[-1] == ',':
                disyuncion = disyuncion[:-1]
            if disyuncion[-1] == ']':
                disyuncion = disyuncion[:-1]
                conjunciones.append(disyuncion)
            else:
                disyunciones.append(disyuncion)
        except IndexError:
            aux = 0

    return [disyunciones,conjunciones]

def getActes(list):
    resultD = {}
    resultC = {}
    if list[0] != []:
        print 'resolving disjunctions'
        disyuncionescsv = list[0][0]
        disyunciones = disyuncionescsv.split(',')
        for element in disyunciones:
            nom = getNom(element)
            actes = esdeveniments.busca(nom)
            print 'searching for',nom,'got',len(actes),'results'
            resultD = set(resultD) | set(actes)

    if list[1] != []:
        print 'resolving conjunctions'
        conjuncionescsv=list[1][0]
        conjunciones = conjuncionescsv.split(',')
        for element in conjunciones:
            nom = getNom(element)
            actes = esdeveniments.busca(nom)
            print 'searching for',nom,'got',len(actes),'results'
            resultC = set(resultC) & set(actes)
    return set(resultD) | set(resultD)
def outputDades(actes):
    file = open('dataQuery.dat','w+')
    for elem in actes:
        nom = elem.find('nom').text
        adreca = elem.find('lloc_simple').find('nom').text
        data = elem.find('data').find('data_proper_acte').text
        s = 'Nom acte:',nom
        file.write(str(s))
        s = '   Adreça:',adreca
        file.write(str(s))

        file.write('    Data: '+data+'\n')


sock = urllib.urlopen("http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199")
xmlSource = sock.read()
sock.close()
esdeveniments = Esdeveniments(xmlSource)

#agafem l'entrada y la dividim en conjuncions
list = parseja(sys.argv[1])

#busqueda de cada element
actes = getActes(list)

print 'total length of the query:',len(actes)
print 'output on file : dataQuery.dat'
outputDades(actes)
print 'done'

#parseig medi transport
transports = sys.argv[2]

sock = urllib.urlopen("http://wservice.viabicing.cat/getstations.php?v=1")
xmlSource = sock.read()
sock.close()
bicing = Bicing(xmlSource)

for elem in actes:
    for coord in elem.iter('googleMaps'):
        lon = float(coord.get('lon'))
        lat = float(coord.get('lat'))
        print len(bicing.getStations([lat,lon],5))