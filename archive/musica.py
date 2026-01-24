import csv
import requests
from bs4 import BeautifulSoup

BASE_URL = "https://www.letras.com/"
BAND_PATHS = [
    "jose-jose"
]

CSV_FILE = "letras_mecano.csv"


def get_band_name(artist_page_url: str) -> str:
    page = requests.get(artist_page_url)
    soup = BeautifulSoup(page.content, 'html.parser')
    h1 = soup.select_one("div.textStyle.--type-title.--size-large.--weight-both.--device-responsive h1")
    return h1.get_text(strip=True) if h1 else "Desconocido"


def get_all_song_links(artist_page_url: str) -> list[tuple[str, str]]:
    page = requests.get(artist_page_url)
    soup = BeautifulSoup(page.content, 'html.parser')
    song_elements = soup.select(".songList-table-content li a.songList-table-songName")
    songs = [(s.text.strip(), s['href']) for s in song_elements if s.get('href')]
    return songs


def get_song_lyrics(song_url_path: str) -> str:
    url = f"{BASE_URL}{song_url_path}"
    page = requests.get(url)
    soup = BeautifulSoup(page.content, 'html.parser')
    div = soup.find("div", class_="lyric-original")
    if not div:
        return ""
    for br in div.find_all("br"):
        br.replace_with("\n")
    for p in div.find_all("p"):
        p.insert_after("\n")
    return div.get_text(separator="\n").strip()


def save_lyrics_to_csv(data: list[tuple[str, str, str, str]], filename: str):
    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(["Banda", "Título", "Letra", "Link"])
        writer.writerows(data)


def main():
    results = []
    for band_path in BAND_PATHS:
        full_url = f"{BASE_URL}{band_path}"
        band_name = get_band_name(full_url)
        songs = get_all_song_links(full_url)
        for title, path in songs:
            full_link = f"{BASE_URL}{path}"
            print(f"Descargando: {title} - {band_name}")
            lyrics = get_song_lyrics(path)
            results.append((band_name, title, lyrics, full_link))
    save_lyrics_to_csv(results, CSV_FILE)
    print(f"Guardado en {CSV_FILE}")


if __name__ == "__main__":
    main()
