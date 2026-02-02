import requests
from bs4 import BeautifulSoup
import csv

# —————— 1) Define aquí los slugs de las personas que quieres scrapear ——————
pages = [
    'Cristiano_Ronaldo',
    'Salvador_Allende',
    'Michelle_Bachelet',
    'Alexis_Sanchez'
    # añade más slugs si lo necesitas…
]

base_url = 'https://es.wikipedia.org/wiki/'

# —————— 2) Prepara el CSV de salida ——————
with open('personas.csv', 'w', encoding='utf-8', newline='') as f:
    writer = csv.writer(f, delimiter=';')
    writer.writerow(['Nombre', 'Descripción', 'URL'])

    # —————— 3) Itera cada página y aplica tu código sin modificar ——————
    for slug in pages:
        link = base_url + slug

        # === TU BLOQUE ORIGINAL ===
        res = requests.get(link)

        soup = BeautifulSoup(res.text, 'html.parser')

        title = soup.find('span', class_ = 'mw-page-title-main').text
         
        data = ''

        for p in soup.find_all('p'):
            data += p.text
            data += '\n'
            
        data = data.strip()
         
        for j in range(3, 467):
            data = data.replace('[' + str(j) + ']', '')

        print(data)
        # ==========================

        # —————— 4) Escribe la fila en el CSV ——————
        # Aquí usamos 'title' como Nombre, 'data' como Descripción y 'link' como URL
        writer.writerow([title, data, link])

print("CSV generado: personas.csv")

print(data)
