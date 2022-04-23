(define-module (tuile taglib)
  #:export
  (
   file-open
   file-close
   file-save
   file-save-id3v1

   get-title
   get-artist
   get-album
   get-comment
   get-genre
   get-year
   get-track

   set-title
   set-artist
   set-album
   set-comment
   set-genre
   set-year
   set-track

   ))


(load-extension "libtag_ti" "init_libtag_ti")


;;
;;
;;;; Reference the tag part from handle.
;;(define tag-ref car)
;;
;;
;;;; ------------------------------------------------------------
;;;; tag access function:
;;
;;;; Open file for tag information and return a handle.
;;(define (tag-open filename)
;;  (let* ((file-handle (taglib-file-new-type (string->pointer filename) 0))
;;         ;; (file-handle (taglib-file-new (string->pointer filename)))
;;         (tag-handle (taglib-file-tag file-handle)))
;;    (cons tag-handle file-handle)))
;;
;;(define (tag-close handle)
;;  ;; (taglib_tag_free_strings)
;;  (taglib-file-free (cdr handle)))
;;
;;(define (tag-save handle)
;;  (taglib-file-save (cdr handle)))
;;
;;(define (tag-save-and-close handle)
;;  (tag-save handle)
;;  (tag-close handle))
;;
;;#;
;;(for-each (lambda (name)
;;            (pr (format #f "(define (tag-get-~a handle)
;;   (pointer->string (taglib-tag-~a (tag-ref handle))))" name name)))
;;          (list "title"
;;                  "artist"
;;                  "album"
;;                  "comment"
;;                  "genre"
;;                  "year"
;;                  "track"))
;;
;;(define (tag-get-title handle)
;;   (pointer->string (taglib-tag-title (tag-ref handle))))
;;
;;(define (tag-get-artist handle)
;;   (pointer->string (taglib-tag-artist (tag-ref handle))))
;;
;;(define (tag-get-album handle)
;;   (pointer->string (taglib-tag-album (tag-ref handle))))
;;
;;(define (tag-get-comment handle)
;;   (pointer->string (taglib-tag-comment (tag-ref handle))))
;;
;;(define (tag-get-genre handle)
;;   (pointer->string (taglib-tag-genre (tag-ref handle))))
;;
;;(define (tag-get-year handle)
;;  (taglib-tag-year (tag-ref handle)))
;;
;;(define (tag-get-track handle)
;;  (taglib-tag-track (tag-ref handle)))
;;
;;
;;#;
;;(for-each (lambda (name)
;;            (pr (format #f "(define (tag-set-~a handle ~a)
;;  (taglib-tag-set-~a (tag-ref handle) (string->pointer ~a)))" name name name name)))
;;          (list "title"
;;                  "artist"
;;                  "album"
;;                  "comment"
;;                  "genre"
;;                  "year"
;;                  "track"))
;;
;;
;;(define (tag-set-title handle title)
;;  (taglib-tag-set-title (tag-ref handle) (string->pointer title)))
;;
;;(define (tag-set-artist handle artist)
;;  (taglib-tag-set-artist (tag-ref handle) (string->pointer artist)))
;;
;;(define (tag-set-album handle album)
;;  (taglib-tag-set-album (tag-ref handle) (string->pointer album)))
;;
;;(define (tag-set-comment handle comment)
;;  (taglib-tag-set-comment (tag-ref handle) (string->pointer comment)))
;;
;;(define (tag-set-genre handle genre)
;;  (taglib-tag-set-genre (tag-ref handle) (string->pointer genre)))
;;
;;(define (tag-set-year handle year)
;;  (taglib-tag-set-year (tag-ref handle) year))
;;
;;(define (tag-set-track handle track)
;;  (taglib-tag-set-track (tag-ref handle) track))
;;
;;
;;
;;
;;
;;;; ------------------------------------------------------------
;;;; libtag_c bindings:
;;
;;;; *****************************************************************************
;;;; File API
;;;; *****************************************************************************
;;
;;;; NOTE: File type not needed in Scheme.
;;
;;;;typedef enum {
;;;;  TagLib_File_MPEG,
;;;;  TagLib_File_OggVorbis,
;;;;  TagLib_File_FLAC,
;;;;  TagLib_File_MPC,
;;;;  TagLib_File_OggFlac,
;;;;  TagLib_File_WavPack,
;;;;  TagLib_File_Speex,
;;;;  TagLib_File_TrueAudio,
;;;;  TagLib_File_MP4,
;;;;  TagLib_File_ASF
;;;;} TagLib_File_Type;
;;
;;
;;
;;;; Creates a TagLib file based on \a filename.  TagLib will try to guess the file
;;;; type.
;;;;
;;;; returns NULL if the file type cannot be determined or the file cannot
;;;; be opened.
;;
;;;; TAGLIB_C_EXPORT TagLib_File *taglib_file_new(const char *filename);
;;(define taglib-file-new
;;  (foreign-library-function "libtag_c" "taglib_file_new"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;
;;;; Creates a TagLib file based on \a filename.  Rather than attempting to guess
;;;; the type, it will use the one specified by \a type.
;;
;;;; TAGLIB_C_EXPORT TagLib_File *taglib_file_new_type(const char *filename, TagLib_File_Type type);
;;(define taglib-file-new-type
;;  (foreign-library-function "libtag_c" "taglib_file_new_type"
;;                            #:return-type '*
;;                            #:arg-types (list '* int)))
;;
;;
;;;; Frees and closes the file.
;;
;;;; TAGLIB_C_EXPORT void taglib_file_free(TagLib_File *file);
;;(define taglib-file-free
;;  (foreign-library-function "libtag_c" "taglib_file_free"
;;                            ;; #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns true if the file is open and readable and valid information for
;;;; the Tag and / or AudioProperties was found.
;;
;;
;;;; TAGLIB_C_EXPORT BOOL taglib_file_is_valid(const TagLib_File *file);
;;(define taglib-file-is-valid
;;  (foreign-library-function "libtag_c" "taglib_file_is_valid"
;;                            #:return-type int
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a pointer to the tag associated with this file.  This will be freed
;;;; automatically when the file is freed.
;;
;;;; TAGLIB_C_EXPORT TagLib_Tag *taglib_file_tag(const TagLib_File *file);
;;(define taglib-file-tag
;;  (foreign-library-function "libtag_c" "taglib_file_tag"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a pointer to the audio properties associated with this file.  This
;;;; will be freed automatically when the file is freed.
;;
;;;; TAGLIB_C_EXPORT const TagLib_AudioProperties *taglib_file_audioproperties(const TagLib_File *file);
;;(define taglib-file-audioproperties
;;  (foreign-library-function "libtag_c" "taglib_file_audioproperties"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Saves the \a file to disk.
;;
;;;; TAGLIB_C_EXPORT BOOL taglib_file_save(TagLib_File *file);
;;(define taglib-file-save
;;  (foreign-library-function "libtag_c" "taglib_file_save"
;;                            #:return-type int
;;                            #:arg-types (list '*)))
;;
;;;; ******************************************************************************
;;;; Tag API
;;;; ******************************************************************************
;;
;;
;;;; Returns a string with this tag's title.
;;;;
;;;; \note By default this string should be UTF8 encoded and its memory should be
;;;; freed using taglib_tag_free_strings().
;;
;;;; TAGLIB_C_EXPORT char *taglib_tag_title(const TagLib_Tag *tag);
;;(define taglib-tag-title
;;  (foreign-library-function "libtag_c" "taglib_tag_title"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a string with this tag's artist.
;;;;
;;;; \note By default this string should be UTF8 encoded and its memory should be
;;;; freed using taglib_tag_free_strings().
;;
;;;; TAGLIB_C_EXPORT char *taglib_tag_artist(const TagLib_Tag *tag);
;;(define taglib-tag-artist
;;  (foreign-library-function "libtag_c" "taglib_tag_artist"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a string with this tag's album name.
;;;;
;;;; \note By default this string should be UTF8 encoded and its memory should be
;;;; freed using taglib_tag_free_strings().
;;
;;;; TAGLIB_C_EXPORT char *taglib_tag_album(const TagLib_Tag *tag);
;;(define taglib-tag-album
;;  (foreign-library-function "libtag_c" "taglib_tag_album"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a string with this tag's comment.
;;;;
;;;; \note By default this string should be UTF8 encoded and its memory should be
;;;; freed using taglib_tag_free_strings().
;;
;;;; TAGLIB_C_EXPORT char *taglib_tag_comment(const TagLib_Tag *tag);
;;(define taglib-tag-comment
;;  (foreign-library-function "libtag_c" "taglib_tag_comment"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns a string with this tag's genre.
;;;;
;;;; \note By default this string should be UTF8 encoded and its memory should be
;;;; freed using taglib_tag_free_strings().
;;
;;;; TAGLIB_C_EXPORT char *taglib_tag_genre(const TagLib_Tag *tag);
;;(define taglib-tag-genre
;;  (foreign-library-function "libtag_c" "taglib_tag_genre"
;;                            #:return-type '*
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns the tag's year or 0 if year is not set.
;;
;;;; TAGLIB_C_EXPORT unsigned int taglib_tag_year(const TagLib_Tag *tag);
;;(define taglib-tag-year
;;  (foreign-library-function "libtag_c" "taglib_tag_year"
;;                            #:return-type unsigned-int
;;                            #:arg-types (list '*)))
;;
;;
;;;; Returns the tag's track number or 0 if track number is not set.
;;
;;;; TAGLIB_C_EXPORT unsigned int taglib_tag_track(const TagLib_Tag *tag);
;;(define taglib-tag-track
;;  (foreign-library-function "libtag_c" "taglib_tag_track"
;;                            #:return-type unsigned-int
;;                            #:arg-types (list '*)))
;;
;;
;;;; Sets the tag's title.
;;;;
;;;; \note By default this string should be UTF8 encoded.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_title(TagLib_Tag *tag, const char *title);
;;(define taglib-tag-set-title
;;  (foreign-library-function "libtag_c" "taglib_tag_set_title"
;;                            #:return-type '*
;;                            #:arg-types (list '* '*)))
;;
;;
;;;; Sets the tag's artist.
;;;;
;;;; \note By default this string should be UTF8 encoded.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_artist(TagLib_Tag *tag, const char *artist);
;;(define taglib-tag-set-artist
;;  (foreign-library-function "libtag_c" "taglib_tag_set_artist"
;;                            #:return-type '*
;;                            #:arg-types (list '* '*)))
;;
;;
;;;; Sets the tag's album.
;;;;
;;;; \note By default this string should be UTF8 encoded.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_album(TagLib_Tag *tag, const char *album);
;;(define taglib-tag-set-album
;;  (foreign-library-function "libtag_c" "taglib_tag_set_album"
;;                            #:return-type '*
;;                            #:arg-types (list '* '*)))
;;
;;
;;;; Sets the tag's comment.
;;;;
;;;; \note By default this string should be UTF8 encoded.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_comment(TagLib_Tag *tag, const char *comment);
;;(define taglib-tag-set-comment
;;  (foreign-library-function "libtag_c" "taglib_tag_set_comment"
;;                            #:return-type '*
;;                            #:arg-types (list '* '*)))
;;
;;
;;;; Sets the tag's genre.
;;;;
;;;; \note By default this string should be UTF8 encoded.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_genre(TagLib_Tag *tag, const char *genre);
;;(define taglib-tag-set-genre
;;  (foreign-library-function "libtag_c" "taglib_tag_set_genre"
;;                            #:return-type '*
;;                            #:arg-types (list '* '*)))
;;
;;
;;;; Sets the tag's year.  0 indicates that this field should be cleared.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_year(TagLib_Tag *tag, unsigned int year);
;;(define taglib-tag-set-year
;;  (foreign-library-function "libtag_c" "taglib_tag_set_year"
;;                            #:return-type '*
;;                            #:arg-types (list '* unsigned-int)))
;;
;;
;;;; Sets the tag's track number.  0 indicates that this field should be cleared.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_set_track(TagLib_Tag *tag, unsigned int track);
;;(define taglib-tag-set-track
;;  (foreign-library-function "libtag_c" "taglib_tag_set_track"
;;                            #:return-type '*
;;                            #:arg-types (list '* unsigned-int)))
;;
;;
;;;; Frees all of the strings that have been created by the tag.
;;
;;;; TAGLIB_C_EXPORT void taglib_tag_free_strings(void);
;;(define taglib-tag-free-strings
;;  (foreign-library-function "libtag_c" "taglib_tag_free_strings"))
;;
