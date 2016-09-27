---
language: rust
contributors:
    - ["P1start", "http://p1start.github.io/"]
translators:
    - ["Ryan Rembert", "http://jrrembert.github.io"]
filename: learnrust-fr.rs
lang: fr-fr
---

Rust est un langage de programmation développé par Mozilla Research. Rust combine le contrôle de bas niveau sur la performance avec la commodité et la sécurité garanties de haut niveau.

Il atteint ces objectifs sans avoir besoin d'un ramasse-miettes ou environnement d'exécution, ce qui rend possible l'utilisation de bibliothèques Rust comme une substitution directe pour C.

La première version de Rust, 0.1, est sortie en janvier 2012 et a tellement évolué rapidement que jusqu'à récemment, l'utilisation de versions stables était déconseillée - à la place ce était conseillé d'utiliser les nightly builds.

Le 15 mai 2015, Rust 1.0 a été libéré avec une garantie complète de compatibilité ascendante. Améliorations aux temps de compilation et d'autres aspects du compilateur sont actuellement disponibles dans la nightly builds. Rust a adopté un modèle de libération à bord du train avec les versions régulières toutes les six semaines. Rust 1.1 beta a été mis à la disposition dans le même temps de la libération de Rust 1.0.

Bien que Rust soit un langage relativement bas niveau, Rust a quelques concepts fonctionnels qui se trouvent généralement dans les langues de niveau supérieur. Cela rend Rust non seulement rapide, mais aussi efficace et facile à coder.

```rust
// Ceci est un commentaire. commentaires de ligne ressemblent à ceci ...
// Et prolonger plusieurs lignes comme celle-ci.

/// Les commentaires de documentation ressemblent à ceci et à soutenir
/// la notation de démarques.
/// # Exemples
///
/// ```
/// let cinq = 5
/// ```

///////////////
// 1. Basics //
///////////////

// Les fonctions
// `I32` est le type 32 bits entiers signés
fn add2(x: i32, y: i32) -> i32 {
    // Retour implicite (pas virgule)
    x + y
}

// Fonction principale
fn main() {
    // Nombres //

    // Reliures immutable
    let x: i32 = 1;

    // Entier suffixes/float
    let y: I32 = 13i32;
    let f: f64 = 1.3f64;

    // Type Inférence
    // La plupart du temps, le compilateur Rust peut déduire le type de variable
    // est, donc vous ne devez pas écrire une annotation de type explicite.
    // Tout au long de ce tutoriel, les types sont explicitement annotées dans
    // de nombreux endroits, mais seulement à des fins de démonstration.
    // L'inférence de type peut gérer cela pour vous la plupart du temps.
    let implicit_x = 1;
    let implicit_f = 1,3;

    // Arithmétique
    let somme = x + y + 13;

    // Variable Mutable
    let mut mutable = 1;
    let mutable = 4;
    let mutable += 2;

    // Chaînes //

    // Littéraux chaîne
    let x: &str = "Bonjour tout le monde!";

    // Impression
    println!("{} {}", f, x); // 1.3 Bonjour tout le monde

    // A `Chaîne` - une chaîne de tas alloué
    let s: String = "Bonjour tout le monde".to_string();

    // Une tranche de chaîne - une vue immutable dans une else chaîne.
    // Ceci est essentiellement un pointeur immutable à une chaîne - il n'a pas
    // contient effectivement le contenu d'une chaîne, juste un pointeur vers
    // quelque chose qui fait(dans ce cas, `s`).
    let s_slice: &str = &s;

    println!("{} {}", s, s_slice); // Bonjour monde Bonjour tout le monde

    // Vecteurs/tableau //

    // Un tableau de taille fixe
    let four_ints: [i32; 4] = [1, 2, 3, 4];

    // Un tableau dynamique(vecteur)
    let mut vecteur: Vec<i32> = vec![1, 2, 3, 4];
    vecteur.push(5);

    // Une tranche - une vue immutable dans un vecteur ou un tableau.
    // Ceci est un peu comme une tranche de chaîne, mais pour les vecteurs.
    let tranche: &[i32] = &vecteur;

    // Utiliser `{:?}` pour imprimer quelque chose de débogage de style
    println!("{:?} {:?}", vecteur, tranche); // [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]

    // Tuples //

    // Un tuple est un ensemble de valeurs de peut-être différents types.
    // de taille fixe
    let x:(i32, &str, f64) = (1, "bonjour", 3.4);

    // Déstructurante `let`
    let (a, b, c) = x;
    println!("{} {} {}", a, b, c); // 1 bonjour 3.4

    // indexation
    println!("{}", x.1); // Bonjour

    //////////////
    // 2. Types //
    //////////////

    // Struct
    struct Point {
        x: i32,
        y: i32,
    }

    let origine: Point = Point { x: 0, y: 0 };

    // Un struct avec des champs sans nom, appelé 'tuple struct'.
    struct Point2(i32, i32);

    let origine2 = Point2(0, 0);

    // Basic C-like enum
    enum Direction {
        Àgauche,
        Droite,
        En_Haut,
        Vers_Le_Bas,
    }

    let en_haut = Direction::En_Haut;

    // Enum avec des champs
    enum OptionnelI32 {
        AnI32(I32),
        Rien,
    }

    let deux: OptionnelI32 = OptionnelI32::AnI32(2);
    let rien = OptionnelI32::Rien;

    // Generics //

    struct Foo<T> { bar: T }

    // Ceci est défini dans la bibliothèque standard comme `Option`.
    enum Optionnel<T> {
        SomeVal(T),
        NoVal,
    }

    // Méthodes //

    impl<T> Foo<T> {
        // Méthodes prennent un paramètre explicite `de self`.
        fn get_bar(self) -> T {
            self.bar
        }
    }

    let a_foo = Foo { bar: 1 };
    println!("{}", a_foo.get_bar()); // 1

    // Traits (connu sous le nom des interfaces ou des classes de types dans
    // d'elses langues).

    trait Frobnicate<T> {
        fn frobnicate(self) -> Option<T>;
    }

    impl<T> Frobnicate<T> for Foo<T> {
        fn frobnicate(self) -> Option<T> {
            Some(self.bar)
        }
    }

    let another_foo = Foo { bar: 1 };
    println!("{:?}", another_foo.frobnicate()); // Some(1)

    /////////////////////////
    // 3. Motif correspondant //
    /////////////////////////

    let foo = OptionnelI32::AnI32(1);
    match foo {
        OptionnelI32::AnI32(n) => println!("Il est un i32: {}", n),
        OptionnelI32::Rien     => println!("Il n'y a rien!"),
    }

    // Motif avancé correspondant
    struct FooBar { x: i32, y: OptionnelI32 }
    let bar = FooBar { x: 15, y: OptionnelI32::AnI32(32) };

    match bar {
        FooBar { x: 0, y: OptionnelI32 :: AnI32(0)} =>
            println!("Les chiffres sont nuls!"),
        FooBar { x: n, y: OptionnelI32 :: AnI32(m)} if n == m =>
            println!("Les chiffres sont les mêmes"),
        FooBar { x: n, y: OptionnelI32 :: AnI32(m)} =>
            println!("Différents numéros: {} {}", n, m)!,
        FooBar { x: _, y: OptionnelI32 :: Rien} =>
            println!("Le deuxième numéro est rien!"),
    }

    /////////////////////
    // 4. Flux de contrôle //
    /////////////////////

    // `for` boucles / itération
    let array = [1, 2, 3];
    for i in array.iter() {
        println!("{}", i);
    }

    // Ranges
    for i in 0u32..10 {
        print!("{}", i);
    }
    println!("");
    // imprime `0 1 2 3 4 5 6 7 8 9`

    // `if`
    if 1 == 1 {
        println!("Maths est travaille!");
    } else {
        println!("Oh non ...!");
    }

    // `if` comme expression
    let valeur = if true {
        "bien"
    } else {
        "mal"
    };

    // `while` boucle
    while 1 == 1 {
        println!("L'univers fonctionne normalement.");
    }

    // Boucle infinie
    loop {
        println!("Bonjour!");
    }

    /////////////////////////////////
    // 5. Sécurité & pointeurs mémoire //
    /////////////////////////////////

    // Pointeur occasion - une seule chose peut "posséder" pointeur à un moment.
    // Cela signifie que lorsque le `Box` laisse son champ d'application, il
    // peut être automatiquement libérée en toute sécurité.
    let mut mien: Box<i32> = Box::new(3);
    *mien = 5; // déréférencer
    // Ici, `now_its_mine` prend possession de` mine`. En d'elses termes,
    // `mien` est déplacé.
    let mut now_its_mine = mien;
    *now_its_mine += 2;

    println!("{}", now_its_mine); // 7
    // println!("{}", de la mine); // Cela ne compile pas parce
    // que `now_its_mine` possède maintenant le pointeur

    // Référence - un pointeur immutable qui fait référence à d'elses données.
    // Quand une référence est prise à une valeur, nous disons que la valeur
    // a été "emprunté".
    // Même si une valeur est emprunté immutablement, il ne peut pas être
    // muté ou déplacé.
    // Un emprunt dure jusqu'à la fin de la portée, il a été créé.
    let mut var = 4;
    var = 3;
    let ref_var: &i32 = &var;

    println!("{}", var); // Contrairement `mien`, `var` peut encore être utilisé
    println!("{}", *ref_var);
    // Var = 5; // Cela ne compile pas parce que `var` est emprunté.
    // *ref_var = 6; // Ce ne serait pas non plus, parce que `ref_var` est une
    // référence immutable.

    // Référence Mutable
    // Même si une valeur est mutably emprunté, il ne peut pas être
    // accessible à tous.
    let mut var2 = 4;
    let ref_var2: &mut i32 = &mut var2;
    // '*' est utilisé pour pointer vers le var2 mutably emprunté.
    *ref_var2 += 2;

    println!("{}", * ref_var2); // 6, // var2 ne serait pas compiler.
    // ref_var2 est de type &mut i32 donc stocke il référence à i32,
    // pas la valeur.
    // var2 = 2; // Cela ne compile pas parce que `var2` est emprunté.
}
```

## Autres lectures

Il y a beaucoup plus à Rust -- ce est juste l'essentiel de Rust afin que vous puissiez comprendre
les choses les plus importantes. Pour en savoir plus sur Rust, lire [La Programmation Rust
Langue](http://doc.rust-lang.org/book/index.html) et etudier la
[/r/rust](http://reddit.com/r/rust) subreddit. Les gens sur le canal de #rust sur
irc.mozilla.org sont aussi toujours prêts à aider les nouveaux arrivants.

Vous pouvez également essayer caractéristiques de Rust avec un compilateur en ligne sur le fonctionnaire
[Rust parc](http://play.rust-lang.org) ou sur la principale
[Site Rust](http://rust-lang.org).
