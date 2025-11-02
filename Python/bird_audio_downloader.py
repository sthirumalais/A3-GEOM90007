import urllib.request, json
import os
import time
import ssl
from typing import Dict, Any, Tuple, Optional

def generate_citation_info(recording: Dict[str, Any]) -> Dict[str, Any]:
    """Generate citation information for Creative Commons licenses"""
    lic = recording.get('lic', '')
    
    # Map license URLs to human-readable names
    license_mapping = {
        '//creativecommons.org/licenses/by-nc-nd/4.0/': 'CC BY-NC-ND 4.0',
        '//creativecommons.org/licenses/by-nc-sa/4.0/': 'CC BY-NC-SA 4.0',
        '//creativecommons.org/licenses/by-nc/4.0/': 'CC BY-NC 4.0'
    }
    
    # Get license type from mapping, fallback to license URL if not found
    license_type = license_mapping.get(lic, lic)
    
    # Build complete citation text for attribution
    citation_text = f"{recording.get('en', 'Unknown species')} ({recording.get('gen', '')} {recording.get('sp', '')})"
    citation_text += f" - Recording by {recording.get('rec', 'Unknown')}"
    citation_text += f" from {recording.get('cnt', 'Unknown location')}"
    citation_text += f" ({recording.get('date', 'Unknown date')})"
    citation_text += f" - Xeno-Canto {recording.get('id', '')}"
    citation_text += f" - Licensed under {license_type}"
    
    return {
        "license_type": license_type,
        "citation_text": citation_text,
        "license_url": lic,
        "recording_url": recording.get('url', ''),
        "recorder": recording.get('rec', 'Unknown')
    }

# Directory to save downloaded audio files and metadata
savePath = "audio_data/"

def search_and_download_recording(searchTerms: str, birdName: str) -> Tuple[Optional[str], Optional[Dict[str, Any]]]:
    """Search for Australian CC licensed recordings and download metadata"""
    # Create directory for this bird species
    path = savePath + birdName.replace(' ', '_').replace(':', '') + "/"
    if not os.path.exists(path):
        print(f"Creating directory: {path}")
        os.makedirs(path)
    
    # Search Xeno-Canto API for recordings globally
    url = f'https://www.xeno-canto.org/api/2/recordings?query={searchTerms.replace(" ", "%20")}&page=1'
    print(f"Searching: {url}")
    
    try:
        # Handle SSL certificate issues
        ssl_context = ssl.create_default_context()
        ssl_context.check_hostname = False
        ssl_context.verify_mode = ssl.CERT_NONE
        
        response = urllib.request.urlopen(url, context=ssl_context)
        data = json.loads(response.read().decode('utf-8'))
        
        # Filter for acceptable CC licenses (non-commercial only)
        acceptable_licenses = [
            '//creativecommons.org/licenses/by-nc-nd/4.0/',  # CC BY-NC-ND 4.0
            '//creativecommons.org/licenses/by-nc-sa/4.0/',  # CC BY-NC-SA 4.0
            '//creativecommons.org/licenses/by-nc/4.0/'       # CC BY-NC 4.0
        ]
        
        cc_recordings = []
        for recording in data.get('recordings', []):
            if recording.get('lic') in acceptable_licenses:
                cc_recordings.append(recording)
        
        print(f"Found {len(cc_recordings)} CC licensed recordings")
        
        if cc_recordings:
            # Select recording
            cc_recordings.sort(key=lambda x: x.get('q', 'Z'))
            best_recording = cc_recordings[0]
            
            # Create  JSON 
            minimal_data = {
                "audio_file": f"{birdName.replace(' ', '_')}_{best_recording['id']}.mp3",
                "citation_info": generate_citation_info(best_recording)
            }
            
            # Save metadata
            with open(f"{path}/single_recording.json", 'w') as f:
                json.dump(minimal_data, f, indent=2)
            
            print(f"Selected: {best_recording.get('lic', 'Unknown')} license")
            return path, best_recording
        else:
            print(f"No acceptable CC recordings found for {birdName}")
            return None, None
            
    except Exception as e:
        print(f"Error searching for {birdName}: {e}")
        return None, None

def download_audio_file(searchTerms: str, birdName: str) -> bool:
    """Download audio file and metadata for a bird species"""
    path, recording_data = search_and_download_recording(searchTerms, birdName)
    
    if not path or not recording_data:
        print(f"Skipping {birdName} - no suitable recordings")
        return False
    
    try:
        # Get download URL and fix format
        file_url = recording_data['file']
        if file_url.startswith('//'):
            download_url = "https:" + file_url
        elif file_url.startswith('http'):
            download_url = file_url
        else:
            download_url = "https://" + file_url
        
        # Download audio file
        audio_filename = f"{birdName.replace(' ', '_')}_{recording_data['id']}.mp3"
        audio_path = f"{path}/{audio_filename}"
        
        print(f"Downloading: {download_url}")
        
        ssl_context = ssl.create_default_context()
        ssl_context.check_hostname = False
        ssl_context.verify_mode = ssl.CERT_NONE
        
        request = urllib.request.Request(download_url)
        with urllib.request.urlopen(request, context=ssl_context) as response:
            with open(audio_path, 'wb') as f:
                f.write(response.read())
        
        print(f"Downloaded: {audio_path}")
        return True
        
    except Exception as e:
        print(f"Error downloading {birdName}: {e}")
        return False

# List of Australian bird species (scientific names)
birds = ['Acanthorhynchus tenuirostris',
 'Microcarbo melanoleucos',
 'Spatula rhynchotis',
 'Cacomantis variolosus',
 'Accipiter fasciatus',
 'Petroica phoenicea',
 'Strepera versicolor',
 'Falco cenchroides',
 'Acanthagenys rufogularis',
 'Coracina novaehollandiae',
 'Poliocephalus poliocephalus',
 'Malurus cyaneus',
 'Dicaeum hirundinaceum',
 'Anhinga novaehollandiae',
 'Chalcites lucidus',
 'Accipiter cirrocephalus',
 'Anthochaera chrysoptera',
 'Pachycephala pectoralis',
 'Acanthiza nana',
 'Porzana fluminea',
 'Lichenostomus melanops',
 'Eopsaltria australis',
 'Cincloramphus mathewsi',
 'Threskiornis moluccus',
 'Parvipsitta porphyrocephala',
 'Pardalotus punctatus',
 'Ardea pacifica',
 'Turdus merula',
 'Sericornis frontalis',
 'Anas gracilis',
 'Bubulcus ibis',
 'Circus approximans',
 'Acanthiza pusilla',
 'Cracticus torquatus',
 'Cygnus atratus',
 'Petrochelidon nigricans',
 'Neophema chrysostoma',
 'Anseranas semipalmata',
 'Stercorarius parasiticus',
 'Tribonyx ventralis',
 'Platalea regia',
 'Falco peregrinus',
 'Circus assimilis',
 'Pelecanus conspicillatus',
 'Grallina cyanoleuca',
 'Hirundapus caudacutus',
 'Calidris acuminata',
 'Acrocephalus australis',
 'Phalacrocorax sulcirostris',
 'Hirundo neoxena',
 'Zanda funerea',
 'Ptilotula fusca',
 'Egretta garzetta',
 'Oriolus sagittatus',
 'Eolophus roseicapilla',
 'Trichoglossus chlorolepidotus',
 'Cacatua sanguinea',
 'Cacatua tenuirostris',
 'Anthus novaeseelandiae',
 'Manorina melanocephala',
 'Turdus philomelos',
 'Platycercus eximius',
 'Epthianura albifrons',
 'Elseyornis melanops',
 'Plegadis falcinellus',
 'Corvus mellori',
 'Acanthiza chrysorrhoa',
 'Oxyura australis',
 'Pycnonotus jocosus',
 'Nymphicus hollandicus',
 'Aythya australis',
 'Gallinago hardwickii',
 'Anas platyrhynchos',
 'Sturnus vulgaris',
 'Ocyphaps lophotes',
 'Hieraaetus morphnoides',
 'Trichoglossus haematodus',
 'Chenonetta jubata',
 'Manorina melanophrys',
 'Charadrius ruficapillus',
 'Passer montanus',
 'Taeniopygia guttata',
 'Rhipidura rufifrons',
 'Phalacrocorax varius',
 'Vanellus miles',
 'Petroica boodang',
 'Anas superciliosa',
 'Gavicalis virescens',
 'Biziura lobata',
 'Parvipsitta pusilla',
 'Columba livia',
 'Psephotus haematonotus',
 'Himantopus himantopus',
 'Elanus axillaris',
 'Melithreptus lunatus',
 'Tadorna tadornoides',
 'Smicrornis brevirostris',
 'Ptilotula penicillata',
 'Podargus strigoides',
 'Hydroprogne caspia',
 'Chloris chloris',
 'Zoothera lunulata',
 'Chlidonias hybrida',
 'Rhipidura leucophrys',
 'Strepera graculina',
 'Colluricincla harmonica',
 'Poodytes gramineus',
 'Chroicocephalus novaehollandiae',
 'Falco longipennis',
 'Porphyrio porphyrio',
 'Phaps chalcoptera',
 'Anas castanea',
 'Artamus personatus',
 'Phalacrocorax fuscescens',
 'Phylidonyris novaehollandiae',
 'Fulica atra',
 'Larus dominicanus',
 'Tyto javanica',
 'Haliastur sphenurus',
 'Rhipidura albiscapa',
 'Pardalotus striatus',
 'Platalea flavipes',
 'Calidris ruficollis',
 'Erythrogonys cinctus',
 'Dacelo novaeguineae',
 'Threskiornis spinicollis',
 'Cisticola exilis',
 'Coturnix ypsilophora',
 'Philemon corniculatus',
 'Thalasseus bergii',
 'Malacorhynchus membranaceus',
 'Ninox novaeseelandiae',
 'Hypotaenidia philippensis',
 'Zosterops lateralis',
 'Gallinula tenebrosa',
 'Acridotheres tristis',
 'Chalcites basalis',
 'Carduelis carduelis',
 'Artamus superciliosus',
 'Morus serrator',
 'Passer domesticus',
 'Cacomantis flabelliformis',
 'Petrochelidon ariel',
 'Gymnorhina tibicen',
 'Anthochaera lunulata',
 'Egretta novaehollandiae',
 'Nycticorax caledonicus',
 'Cracticus nigrogularis',
 'Alauda arvensis',
 'Neochmia temporalis',
 'Ardea alba',
 'Phalacrocorax carbo',
 'Cacatua galerita',
 'Tachybaptus novaehollandiae',
 'Myzomela sanguinolenta',
 'Glossopsitta concinna',
 'Ninox strenua',
 'Acanthiza lineata',
 'Larus pacificus',
 'Heteroscenes pallidus',
 'Falco berigora',
 'Platycercus elegans',
 'Chalcites osculans',
 'Pachycephala rufiventris',
 'Ardea intermedia',
 'Eudynamys orientalis',
 'Corvus coronoides',
 'Anthochaera carunculata',
 'Todiramphus sanctus']

def download_all_birds() -> Tuple[int, int]:
    """Download CC licensed audio files for all bird species"""
    successful_downloads = 0
    failed_downloads = 0
    
    print(f"Downloading {len(birds)} bird species...")
    print("=" * 60)
    
    for i, bird in enumerate(birds, 1):
        print(f"\n[{i}/{len(birds)}] Processing: {bird}")
        
        # Search for call recordings only
        success = download_audio_file(f"{bird} type:call", bird)
        
        if success:
            successful_downloads += 1
            print(f"Downloaded {bird}")
        else:
            failed_downloads += 1
            print(f"Failed {bird}")
        
        time.sleep(1)
    
    print("\n" + "=" * 60)
    print("DOWNLOAD SUMMARY:")
    print(f"Total birds: {len(birds)}")
    print(f"Successful: {successful_downloads}")
    print(f"Failed: {failed_downloads}")
    print(f"Success rate: {(successful_downloads/len(birds)*100):.1f}%")
    
    return successful_downloads, failed_downloads

def generate_citation_report() -> Dict[str, Any]:
    """Generate summary report of all downloaded recordings"""
    report = {
        "total_recordings": 0,
        "license_breakdown": {},
        "citations": []
    }
    
    # Scan all downloaded recordings
    for root, _, files in os.walk(savePath):
        for file in files:
            if file == "single_recording.json":
                try:
                    with open(os.path.join(root, file), 'r') as f:
                        data = json.load(f)
                    
                    citation_info = data.get('citation_info', {})
                    license_type = citation_info.get('license_type', 'Unknown')
                    
                    report["total_recordings"] += 1
                    
                    if license_type not in report["license_breakdown"]:
                        report["license_breakdown"][license_type] = 0
                    report["license_breakdown"][license_type] += 1
                    
                    report["citations"].append(citation_info.get('citation_text', ''))
                    
                except Exception as e:
                    print(f"Error reading {file}: {e}")
    
    # Save report
    with open(f"{savePath}/citation_report.json", 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\nCitation report saved: {savePath}/citation_report.json")
    print(f"Total recordings: {report['total_recordings']}")
    print("License breakdown:")
    for license_type, count in report["license_breakdown"].items():
        print(f"  {license_type}: {count}")
    
    return report

# Main execution
if __name__ == "__main__":
    download_all_birds()
    generate_citation_report()
    print("Download completed successfully!")