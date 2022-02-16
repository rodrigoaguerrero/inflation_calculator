import urllib.request
pdf_path = "/Users/rodrigoguerrero/Documents/School/ISTA 498/CBSA_maps"

def download_file(download_url, filename):
    response = urllib.request.urlopen(download_url)    
    file = open(filename + ".pdf", 'wb')
    file.write(response.read())
    file.close()
 
download_file(pdf_path, "Test")