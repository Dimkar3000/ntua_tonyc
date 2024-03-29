# Σκοπός

Η δημιουργία ενός compiler για την γλώσσα 'Tony' που δόθηκε ως εργασία στους μαθητές του ECE NTUA για το μάθημα των compilers το έτος 2020.

# Η γλώσσα Tony

Μια απλή γλώσσα προστακτικού προγραμματισμού. Τα κύρια χαρακτηριστικά της εν συντομία είναι τα εξής:

- Απλή δομή και σύνταξη εντολών και εκφράσεων που μοιάζει με αυτήν κάποιων γλωσσών σεναρίων.
- Βασικοί τύποι δεδομένων για λογικές τιμές, χαρακτήρες, ακέραιους αριθμούς, μονοδιάστατους πίνακες και λίστες.
- Απλές συναρτήσεις, πέρασμα κατ’ αξία ή κατ’ αναφορά.
- Εμβέλεια μεταβλητών όπως στην Pascal.
- Βιβλιοθήκη συναρτήσεων.

Στο φάκελο `files` υπάρχει η εκφώνηση της άσκησης μαζί με ότι άλλο δόθηκε για την εργασία. Επίσης, έχει τον φάκελο `examples` με προγράμματα που μπορούν να γίνουν compile ή που γυρνάνε συγκεκριμένα errors.

# Εγκατάσταση

## Προαπετούμενα

- Rust
- LLVM 10
- Τον **Boehm-Demers-Weiser** garbage collector

Θα χρειαστείς να έχεις την rust εγκαταστημένη στο σύστημα σου για να κάνεις compile το project. Επίσης, θα χρειαστείς την μεταβλητή `LLVM_SYS_100_PREFIX=/llvm/path/` στο path (Windows) ή να τρέξεις το cargo έτσι:

```bash
cd your/crate/path
LLVM_SYS_100_PREFIX=llvm-10.0.0.src\build\install cargo build
```

### Windows

- You will need to compile `llvm 10` for this project to work. Follow [this](https://crates.io/crates/llvm-sys) instuction to compiler llvm and set it up for this project.

- I used the Boehm-Demers-Weiser garbage collector (8.0.4). You will need to compile it and get the compiled file `gcmt-lib.lib` on the root of the rust project, next to **_cargo.toml_**.The library was built on release mode with /MT flag for msvc on windows. Get it [here](https://github.com/ivmai/bdwgc).

### Linux

- I am not sure if you need to compile llvm but I haven't tested the project with Release binaries. Technicaly the only requirements are a llvm installation with llvm-config.
- You can install the collector with your package manager (libgc). If you build from **_source_** run make with **_"root"_** so the linker can find it in a global library directory

# Προβλήματα (English)

1. In case of linking errors look at the clink file of your platform.

   - On Windows the gc is passed as a file called libgcmt.lib
   - The standard library is written in c++

2. Run the application from the root of the project. You can use the command

   ```bash
   cargo run --release -- file/to/compile
   ```

   in order to run the compiler **_or_** get the files

   - libgcmt.lib (Windows Only)
   - clink.{bat,sh} (Linking Script)
   - libtonystd.cpp (Standard Library)
   - tonyc.exe (The Compiler)

   in the folder you want to run the compiler from. The complier is stored in the directory ```/target/release/``` after a successful built
