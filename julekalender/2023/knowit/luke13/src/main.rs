// Prime numbers were taken from: https://github.com/srmalins/primelists/tree/master
// And put in one file with cat primes.{0000..0100} > primes10m.txt

// For i in alver_på_jobb, tenn vindu
// For i in alver_ikke_på_jobb, sjekk om de slipper unna til liste alver_slapp_unna
// For i in grinchen, slukk vinduer
// For i in alver_slapp_unna, lag liste alver_straffet med dem som ikke lenger slipper unna
//
// Vinduer er indeksert fra 0. Det er 400 009 vinduer, 0..=400008
// Alve-id starter også på 0
// Primtall starter på 1
//
// Tente vinduer:
// Første vindu: (id * 2) % antall_vinduer
// Andre  vindu: (id + primes[id + 1]) & antall_vinduer
fn main() {
    let alver_på_jobb: Vec<usize> = std::fs::read_to_string("src/alver_på_jobb.txt")
        .expect("hmm")
        .trim()
        .lines()
        .map(|l| l.parse().unwrap_or(0))
        .collect();

    let alver_ikke_på_jobb: Vec<usize> = std::fs::read_to_string("src/alver_ikke_på_jobb.txt")
        .expect("hmm2")
        .trim()
        .lines()
        .map(|l| l.parse().unwrap_or(0))
        .collect();

    let grinchen: Vec<usize> = std::fs::read_to_string("src/grinchen.txt")
        .expect("hmm3")
        .trim()
        .lines()
        .map(|l| l.parse().unwrap_or(0))
        .collect();

    let primes: Vec<usize> = std::fs::read_to_string("../primelists/100000primes/primes10m.txt")
        .expect("hmm4")
        .trim()
        .lines()
        .map(|l| l.trim().parse().unwrap_or(0)) // AAA dette var visst feilen hele tiden
                                                // sfhljadshfljbvcibfibaiuabdflbadsclkjnafheuqhfibcpiaubk
        .collect();

    // println!("{:?}", primes.split_at(100).0);

    // let alver_på_jobb: Vec<usize> = vec![0, 6];
    // let alver_ikke_på_jobb: Vec<usize> = vec![1, 13];
    // let grinchen: Vec<usize> = vec![5];

    // let antall_vinduer: usize = 7;
    let antall_vinduer: usize = 400009;
    let mut vinduer: Vec<bool> = vec![false; antall_vinduer];
    let mut alver_slapp_unna: Vec<usize> = Vec::new();
    let mut alver_straffet: Vec<usize> = Vec::new(); // Svaret er lengden av denne

    // println!("Vinduer start: {:?}", vinduer);

    // Tenn vinduene
    for id in alver_på_jobb {
        let vindu1: usize = (id * 2) % antall_vinduer;
        let vindu2: usize = (id + primes[id]) % antall_vinduer;
        vinduer[vindu1] = true;
        vinduer[vindu2] = true;
        // println!("Id: {}, Brukt primtall: {}, Vindu 1: {}, Vindu 2: {}", id, primes[id], vindu1, vindu2);
    }

    // println!("Prime nr. 7 (primes[6]): {}", primes[6]);

    // println!("Vinduer: {:?}", vinduer);

    // Finn alver som slapp unna
    for id in alver_ikke_på_jobb {
        let vindu1: usize = (id * 2) % antall_vinduer;
        let vindu2: usize = (id + primes[id]) % antall_vinduer;
        if vinduer[vindu1] && vinduer[vindu2] {
            alver_slapp_unna.push(id);
        }
    }

    // println!("Alver som slapp unna: {:?}", alver_slapp_unna);

    for vindu in grinchen {
        vinduer[vindu] = false;
    }

    // println!("Vinduer etter slukking: {:?}", vinduer);

    for id in alver_slapp_unna {
        let vindu1: usize = (id * 2) % antall_vinduer;
        let vindu2: usize = (id + primes[id]) % antall_vinduer;
        if !vinduer[vindu1] || !vinduer[vindu2] {
            alver_straffet.push(id);
        }
    }

    // println!("Alver som ble straffet: {:?}", alver_straffet);

    println!("Antall alver som ikke slapp unna: {}", alver_straffet.len());
}
