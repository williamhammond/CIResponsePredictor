import http.client
import csv

import urllib.parse
import urllib.request

import requests

def get_neighborhood(latitude, longitude):
    url = "https://maps.googleapis.com/maps/api/geocode/json"
    key = " AIzaSyAFj0I1gCaquVaj2asX_cdyHqOQBDGqGnQ"

    payload = {"latlng":str(latitude)+","+str(longitude), "key":key}
    r = requests.get(url, params=payload)
    if r.status_code >= 400:
        print(r.status_code)
    try:
        result = r.json()["results"][1]["formatted_address"].split(',')[0]
    except IndexError:
        print(r.json())
        result = -1
    return result 


def get_neighborhood2(longitude, latitude):
    url = "https://maps.googleapis.com/maps/api/geocode/json"
    params = urllib.parse.urlencode({"latlng":str(latitude)+","+str(longitude)})
    urllib.request(url, data=params)

    return conn.getresponse()



results = []
with open('../data/records.csv') as csvFile:
    coordReader = csv.DictReader(csvFile)
    next(coordReader)
    for coord in coordReader:
        hood = get_neighborhood(float(coord['long']), float(coord['lat']))
        if hood != -1:
            results.append({'eventID': coord['eventID'], 'neighborhood':hood})

with open('../data/hood.csv', 'w', ) as csvfile:
    fieldnames = ["eventID", "neighborhood"]
    hoodwriter = csv.DictWriter(csvfile, fieldnames=fieldnames)

    hoodwriter.writeheader()
    for result in results:
        hoodwriter.writerow(result)
