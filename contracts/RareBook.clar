;; RareBook: Literary Heritage and Manuscript Authentication Platform
;; Version: 1.0.0

(define-constant ERR-PERMISSION-DENIED (err u1))
(define-constant ERR-BOOK-NOT-FOUND (err u2))
(define-constant ERR-DUPLICATE-ENTRY (err u3))
(define-constant ERR-INVALID-CONDITION (err u4))
(define-constant ERR-INVALID-PUBLICATION-YEAR (err u5))
(define-constant ERR-INVALID-GENRE (err u6))
(define-constant ERR-INVALID-RARITY (err u7))
(define-constant ERR-INVALID-TITLE (err u8))
(define-constant ERR-INVALID-AUTHOR (err u9))

(define-constant MIN-PUBLICATION-YEAR u1450)

(define-data-var next-book-id uint u1)

(define-map rare-books
    uint
    {
        librarian: principal,
        book-title: (string-utf8 100),
        author: (string-utf8 120),
        genre: (string-utf8 30),
        rarity: (string-utf8 20),
        condition: (string-utf8 15),
        publication-year: uint
    }
)

(define-private (validate-genre (genre (string-utf8 30)))
    (or 
        (is-eq genre u"Literature")
        (is-eq genre u"History")
        (is-eq genre u"Philosophy")
        (is-eq genre u"Science")
        (is-eq genre u"Poetry")
        (is-eq genre u"Biography")
    )
)

(define-private (validate-rarity (rarity (string-utf8 20)))
    (or 
        (is-eq rarity u"Extremely Rare")
        (is-eq rarity u"Very Rare")
        (is-eq rarity u"Rare")
        (is-eq rarity u"Uncommon")
        (is-eq rarity u"Scarce")
    )
)

(define-private (validate-string-format (text (string-utf8 120)) (min-chars uint) (max-chars uint))
    (let 
        (
            (text-chars (len text))
        )
        (and 
            (>= text-chars min-chars)
            (<= text-chars max-chars)
        )
    )
)

(define-public (catalog-rare-book 
    (book-title (string-utf8 100))
    (author (string-utf8 120))
    (genre (string-utf8 30))
    (rarity (string-utf8 20))
    (publication-year uint)
)
    (let
        (
            (book-id (var-get next-book-id))
        )
        (asserts! (validate-string-format book-title u2 u100) ERR-INVALID-TITLE)
        (asserts! (validate-string-format author u3 u120) ERR-INVALID-AUTHOR)
        (asserts! (>= publication-year MIN-PUBLICATION-YEAR) ERR-INVALID-PUBLICATION-YEAR)
        (asserts! (validate-genre genre) ERR-INVALID-GENRE)
        (asserts! (validate-rarity rarity) ERR-INVALID-RARITY)
        
        (map-set rare-books book-id {
            librarian: tx-sender,
            book-title: book-title,
            author: author,
            genre: genre,
            rarity: rarity,
            condition: u"archived",
            publication-year: publication-year
        })
        (var-set next-book-id (+ book-id u1))
        (ok book-id)
    )
)

(define-public (loan-book (book-id uint))
    (let
        (
            (book (unwrap! (map-get? rare-books book-id) ERR-BOOK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get librarian book)) ERR-PERMISSION-DENIED)
        (asserts! (is-eq (get condition book) u"archived") ERR-INVALID-CONDITION)
        (ok (map-set rare-books book-id (merge book { condition: u"on-loan" })))
    )
)

(define-read-only (get-book-record (book-id uint))
    (ok (map-get? rare-books book-id))
)

(define-read-only (get-librarian (book-id uint))
    (ok (get librarian (unwrap! (map-get? rare-books book-id) ERR-BOOK-NOT-FOUND)))
)