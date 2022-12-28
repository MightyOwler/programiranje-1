import csv
import os
import requests
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'TODO'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'TODO'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'TODO'


def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    
    try:
        # del kode, ki morda sproži napako
        response = requests.get(url, headers={
        # "Accept-Language": "sl-si"
        })
        page_content = response.text
    except Exception as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(f"NAPAKA PRI PRENOSU: {url} ::", e)
        return(None)
    # nadaljujemo s kodo če ni prišlo do napake
    
    return page_content
    
    
    # with open("podatki_z_bolhe.html", 'w') as dat:
    #     dat.write(page_content)
    

def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    
    # directory = "", filename = "podatki_z_bolhe.html"
    
    vsebina = download_url_to_string(page)
    save_string_to_file(vsebina, directory, filename)


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    """Funkcija vrne celotno vsebino datoteke "directory"/"filename" kot niz."""
    
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file_out:
        return file_out.read()


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    """Funkcija poišče posamezne oglase, ki se nahajajo v spletni strani in
    vrne seznam oglasov."""
    
    vzorec_za_oglase = re.compile(
        r"<article>.*?</article>",
        flags=re.DOTALL
    )

    oglasi = []
    
    for oglas in vzorec_za_oglase.finditer(page_content):
        oglasi.append(oglas.group(0))
        
    return oglasi

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, lokaciji, datumu objave in ceni v oglasu.


def get_dict_from_ad_block(block):
    """Funkcija iz niza za posamezen oglasni blok izlušči podatke o imenu, ceni
    in opisu ter vrne slovar, ki vsebuje ustrezne podatke."""
    vzorec_podatkov_v_oglasu = re.compile(
        r"<article>.*?<a name=.*?>(?P<ime>.*?)</a>.*?"
        r'<span class="entity-description-itemCaption">Lokacija: </span>(?P<lokacija>.*?)<br>.*?</div>.*?'
        r'<div class="entity-pub-date">.*?<time.*?">(?P<datum>.*?)</time></div>.*?'
        r'<div class="entity-prices">.*?<strong class="price.*?>(?P<cena>.*?)&nbsp;.*?<span>',
        flags=re.DOTALL
    )
    
    slovar = []
    for macka in vzorec_podatkov_v_oglasu.finditer(block):
        slovar.append({
            'ime': macka.groupdict()['ime'],
            'datum': macka.groupdict()['datum'],
            'lokacija': macka.groupdict()['lokacija'],
            'cena': macka.groupdict()['cena'] + "€",
        })
    return slovar 
        

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    """Funkcija prebere podatke v datoteki "directory"/"filename" in jih
    pretvori (razčleni) v pripadajoč seznam slovarjev za vsak oglas posebej."""
    
    string = read_file_to_string(directory, filename)
    seznam_slovarjev = []
    
    for blok in page_to_ads(string):
        seznam_slovarjev.append(get_dict_from_ad_block(blok))
        
    
    return seznam_slovarjev


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da so ključi vseh
    slovarjev parametra ads enaki in je seznam ads neprazen."""
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    
    


# Celoten program poženemo v glavni funkciji

# Najprej naredimo posamezne funkcije, ki vse naredijo

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran

    # Iz lokalne (html) datoteke preberemo podatke

    # Podatke preberemo v lepšo obliko (seznam slovarjev)

    # Podatke shranimo v csv datoteko

    # Dodatno: S pomočjo parametrov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prenese (četudi že obstaja)
    # in enako za pretvorbo

    raise NotImplementedError()


# Ta program se izvede samo, če je to file, ki ga poženemo!
# Če importamo ta file, se ne bo pognalo!

if __name__ == '__main__':
    main()
