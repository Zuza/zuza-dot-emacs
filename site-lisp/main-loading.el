(ido-mode t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; alt + up/down/left/right
(windmove-default-keybindings 'meta)

;; disable the annoying C-w
(put 'kill-region 'disabled t)

;; truncate lines that are longer then the buffer window (don't wrap)
(setq truncate-lines t)

(setq default-tab-width 2)
(setq compilation-ask-about-save nil)

;; org mode
(add-to-list 'load-path "~/site-lisp/org-7.7/lisp")
(add-to-list 'load-path "~/site-lisp/org-7.7/contrib/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(defmacro safe-var (x)
	"Safely retrieves the variable if it exists. Suppres the error if it doesn't."
	(list 'condition-case 'err
				x
			(list 'error 'nil)
	)
)

(defun revert-all-buffers()
	"Refreshs all open buffers from their respective files"
	(interactive)
	(let* ((list (buffer-list))
				 (buffer (car list)))
		(while buffer
			(if (string-match "\\*" (buffer-name buffer)) 
					(progn
						(setq list (cdr list))
						(setq buffer (car list)))
	      (progn
	        (set-buffer buffer)
	        (revert-buffer t t t)
	        (setq list (cdr list))
	        (setq buffer (car list))))))
	(message "Refreshing open files")
)

(defun save-and-quick-compile () (interactive)
	(save-buffer 0)
	(if (file-readable-p "Makefile")
		(compile (concat "make " (safe-var make-command)))
		(compile (concat "g++ " (buffer-file-name) " -O2 -Wall " (safe-var compile-suffix)))
	)
)

(defun zuza-quick-run () (interactive)
	(save-buffer 0)
	(shell-command "./a.out &")
)

(defun zuza-quick-kill () (interactive)
	(kill-process "*Async Shell Command*")
)

(defun zuza-c++-mode-hook ()
	(local-set-key "\M-2" 'save-and-quick-compile)
	(local-set-key "\M-3" 'zuza-quick-run)
	(local-set-key "\M-4" 'zuza-quick-kill)
	(local-set-key "\M-n" 'next-error)
	(local-set-key "\M-p" 'previous-error)
)

(add-hook 'c++-mode-hook 'zuza-c++-mode-hook)

;; (make-variable-buffer-local 'exe-name)
;; (make-variable-buffer-local 'quick-compile-command)

;; (defun remove-suffix (name)
;;   "Remove any extension from a file name"
;;   (if (string-match "\\(.*\\)\\." name)
;;       (substring name (match-beginning 1) (match-end 1))
;;     name))
       
;; (defun save-and-quick-compile (&optional arg) (interactive)
;;   (save-buffer arg)
;;   (if (file-readable-p "./Makefile")
;; 		(compile "make")
;; 	(compile (concat "gxx -o \"" exe-name "\" \"" buffer-file-name "\" -O2 -Wall -lm")))
;; )

;; (defun smart-tab (&optional arg) (interactive)
;;   (setq pos (point))
;;   (beginning-of-line)
;;   (if (equal pos (point))
;; 		(c-indent-command)
;; 	 (goto-char pos) (dabbrev-expand arg))
;;   )

;; (defun pblank (tmp text) (interactive) 
;;   (c-indent-command) (insert tmp)
;;   (setq ret (read-input text))
;;   (delete-backward-char (length tmp) )
;; )

;; (defun pitaj (tmp text) (interactive) 
;;   (c-indent-command) (insert tmp)
;;   (setq ret (read-no-blanks-input text))
;;   (delete-backward-char (length tmp) )
;; )

;; (defun luka-for (i lo hi) (interactive) 
;;   (insert "for( int " )
;;   (when (eq i nil ) (pitaj "" "Ime varijable: " ) (setq i ret) )
;;   (insert i " = " )
;;   (when (eq lo nil ) (pitaj "" "Donja granica: " ) (setq lo ret) )
;;   (insert lo "; " i " < " )
;;   (when (eq hi nil ) (pitaj "" "Gornja granica: " ) (setq hi ret) )
;;   (insert hi "; ++" i " )" )
;; )

;; (defun luka-for1 () (interactive) (luka-for nil "0" "n" ) )
;; (defun luka-for2 () (interactive) (luka-for nil "0" nil ) )
;; (defun luka-for3 () (interactive) (luka-for nil nil nil ) )

;; (defun zuza-forall () (interactive) 
;;   (pitaj "" "Ime varijable: " ) (setq counter ret)
;;   (insert "for( int " counter " = 0; " counter " < ( int )" )
;;   (pitaj "" "Ime containera: " ) (setq V ret)
;;   (insert V ".size(); ++" counter " )" )
;; )

;; (defun luka-forit (tip it cont) (interactive) 
;;   (insert "for( " )
;;   (when (eq tip nil ) (pblank "" "Tip containera: " ) (setq tip ret) )
;;   (insert tip "::iterator " )
;;   (when (eq it nil ) (pitaj "" "Ime iteratora: " ) (setq it ret) )
;;   (insert it " = " )
;;   (when (eq cont nil ) (pitaj "" "Container: " ) (setq cont ret) )
;;   (insert cont ".begin(); " it " != " cont ".end(); ++" it " )" ) 
;; )

;; (defun luka-forit2 () (interactive) (luka-forit nil "it" nil ) )
;; (defun luka-forit3 () (interactive) (luka-forit nil nil nil ) )

;; (defun luka-manje () (interactive) 
;;   (c-indent-command) (insert "bool operator < ( const " )
;;   (pitaj "" "Ime structa: " ) (setq struct ret)
;;   (insert struct " &A, const " struct " &B ) {\n" )
;;   (c-indent-command) (insert "if( A." );

;;   (pitaj "" "Ime varijable (nista za prekid): " ) (setq a ret)
;;   (while (not (string= a "")) 
;; 	 (insert a " != B." a " ) return A." a " < B." a ";\n")
;; 	 (c-indent-command) (insert "if( A.")
;; 	 (pitaj "" "Ime varijable (nista za prekid): " ) (setq a ret)
;;   )
;;   (delete-char -6 )
;;   (insert "return false;\n}" )
;;   (c-indent-command) (insert "\n" )
;;   (c-indent-command)
;; )
;; (defun luka-all() (interactive)
;;   (pitaj "all( " "Container: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end()" )
;; )
;; (defun luka-sort () (interactive)
;;   (insert "sort( " )
;;   (pitaj "" "Container: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end() );" )
;; )
;; (defun luka-reverse () (interactive)
;;   (insert "reverse( " )
;;   (pitaj "" "Container: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end() );" )
;; )
;; (defun luka-permute() (interactive)
;;   (insert "sort( " )
;;   (pitaj "" "Container: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end() );\n" )
;;   (c-indent-command) (insert "do {\n\n} " )
;;   (c-indent-command) (insert "while( next_permutation( " a ".begin(), " a ".end() ) );\n" )
;;   (previous-line 2) (c-indent-command) 
;; )
;; (defun luka-set-union() (interactive)
;;   (insert "set_union( " )
;;   (pitaj "" "Prvi set: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end(), " )
;;   (pitaj "" "Drugi set: " ) (setq b ret)
;;   (insert b ".begin(), " b ".end(), inserter( " )
;;   (pitaj "" "Treci set: " ) (setq c ret)
;;   (insert c ", " c ".begin() ) );" )
;; )
;; (defun luka-set-intersection() (interactive)
;;   (insert "set_intersection( " )
;;   (pitaj "" "Prvi set: " ) (setq a ret)
;;   (insert a ".begin(), " a ".end(), " )
;;   (pitaj "" "Drugi set: " ) (setq b ret)
;;   (insert b ".begin(), " b ".end(), inserter( " )
;;   (pitaj "" "Treci set: " ) (setq c ret)
;;   (insert c ", " c ".begin() ) );" )
;; )
;; (defun luka-memnull () (interactive)
;;   (insert "memset( " )
;;   (pitaj "" "Array: " ) (setq a ret)
;;   (insert a ", 0, sizeof " a " );" )
;; )
;; (defun luka-meminf () (interactive)
;;   (insert "memset( " )
;;   (pitaj "" "Array: " ) (setq a ret)
;;   (insert a ", 0x3F, sizeof " a " );" )
;; )
;; (defun zuza-refxy () (interactive)
;;   (insert "[" )
;;   (pitaj "" "Point: " ) (setq a ret)
;;   (insert a ".X][" a ".Y]" )
;; )
;; (defun zuza-memminus () (interactive)
;;   (insert "memset( " )
;;   (pitaj "" "Array: " ) (setq a ret)
;;   (insert a ", -1, sizeof " a " );" )
;; )
;; (defun zuza-valid () (interactive)
;;   (insert "inline bool valid( int x, int y ) { if( x < 0 || y < 0 || x >= " )
;;   (pitaj "" "X-limit: " ) (setq a ret)
;;   (insert a " || y >= ")
;;   (pitaj "" "Y-limit: " ) (setq b ret)
;;   (insert b " ) return false; return true; }" )
;; )
;; (defun zuza-modpower () (interactive)
;;   (pblank "" "Tip: " ) (setq a ret)
;;   (insert a " power_mod( " a " b, " a " e, " a " mod )\n{\n    if( e == 0 ) return 1 % mod;\n    if( e == 1 ) return b % mod;\n    if( e&1 ) return ( power_mod( b, e-1, mod ) * b ) % mod;\n    " a " tmp = power_mod( b, e >> 1, mod ); return ( tmp * tmp ) % mod;\n}" )
;; )

;; (defun zuza-scanint () (interactive)
;;   (pitaj "" "Ime varijable: " ) (setq a ret)
;;   (insert "int " a "; scanf( \"%d\", &" a " );" )
;; )

;; (defun zuza-lthen () (interactive)
;;   (pitaj "" "A: " ) (setq a ret)
;;   (insert a " + EPS < ")
;;   (pitaj "" "B: " ) (setq b ret)
;;   (insert b )
;; )

;; (defun zuza-lte () (interactive)
;;   (pitaj "" "A: " ) (setq a ret)
;;   (insert a " <= ")
;;   (pitaj "" "B: " ) (setq b ret)
;;   (insert b " + EPS" )
;; )

;; (defun zuza-gthen () (interactive)
;;   (pitaj "" "A: " ) (setq a ret)
;;   (insert a " > ")
;;   (pitaj "" "B: " ) (setq b ret)
;;   (insert b " + EPS" )
;; )

;; (defun zuza-gte () (interactive)
;;   (pitaj "" "A: " ) (setq a ret)
;;   (insert a " + EPS >= ")
;;   (pitaj "" "B: " ) (setq b ret)
;;   (insert b )
;; )

;; (defun zuza-unique () (interactive)
;;   (pitaj "" "Ime varijable: " ) (setq a ret)
;;   (c-indent-command) (insert "sort( " a ".begin(), " a ".end() );\n" )	
;;   (c-indent-command) (insert a ".resize( unique( " a ".begin(), " a ".end() ) - " a ".begin() );" )
;; )

;; (defun zuza-main () (interactive)
;;   (insert "int main( void )\n{\n    return (0-0);\n}\n" )
;;   (backward-char 21)
;;   (c-indent-command)
;; )

;; (defun zuza-bins () (interactive)
;;   (pitaj "" "Ime vektora: " ) (setq a ret)
;;   (c-indent-command) (insert "back_inserter( " a " )" )
;; )

;; (defun zuza-sins () (interactive)
;;   (pitaj "" "Ime seta: " ) (setq a ret)
;;   (c-indent-command) (insert "inserter( " a ", " a ".begin() )" )
;; )

;; (defun zuza-power () (interactive)
;;   (pblank "" "Tip: " ) (setq a ret)
;;   (insert a " power( " a " b, " a " e )\n{\n    if( e == 0 ) return 1;\n    if( e == 1 ) return b;\n    if( e&1 ) return power( b, e-1 ) *  b;\n    " a " tmp = power( b, e >> 1 ); return tmp * tmp;\n}" )
;; )

;; (defun skel-forn () (interactive) (luka-for nil "0" "n" ) )
	
;; (defun quick-run () (interactive)
;;   (shell-command (concat "\"" exe-name "\"")))

;; (defun my-c-mode-hook () (interactive)
;;   (setq c-tab-always-indent t) ;; Tab retabifies    
;;   (setq indent-tabs-mode nil) ;; spejsovi, ne tabovi
;;   (setq c-basic-offset 2) ;; tab 4
;;   (setq truncate-lines t)
;;   (setq max-mini-window-height 10)
;;   (setq dabbrev-case-fold-search nil)
;; ;;  (c-toggle-hungry-state 1)
;;   (setq exe-name (concat (remove-suffix buffer-file-name) ".exe"))
;;   (define-key c-mode-base-map "\M-2" 'save-and-quick-compile)
;;   (define-key c-mode-base-map "\M-3" 'quick-run)
;;   (define-abbrev c++-mode-abbrev-table "fori" "for( int i = 0; i < n; ++i )" )
;;   (define-abbrev c++-mode-abbrev-table "forj" "for( int j = 0; j < n; ++j )" )
;;   (define-abbrev c++-mode-abbrev-table "fork" "for( int k = 0; k < n; ++k )" )
;;   (define-abbrev c++-mode-abbrev-table "pb" "push_back" )
;;   (define-abbrev c++-mode-abbrev-table "pf" "push_front" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiinspiraciju" "// ......+---+....\n// ...../   /|....\n// ....+---+ |....\n// ....|   | +....\n// ....|   |/|....\n// ....+---+ |....\n// ....|   | +....\n// ....|   |/|....\n// ....+---+ |....\n// ..+-|   | +---+\n// ./  |   |/   /|\n// +---+---+---+ |\n// |   |   |   | +\n// |   |   |   |/.\n// +---+---+---+.." )
;;   (define-abbrev c++-mode-abbrev-table "tcinclude" "#include <algorithm>\n#include <functional>\n\n#include <cmath>\n#include <cstdio>\n#include <cstdlib>\n#include <cstring>\n\n#include <vector>\n#include <string>\n#include <sstream>\n#include <iostream>\n\nusing namespace std;" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiroot" "int root( int x ) \n{\n    long long l = 1, r = x;\n\n    while( l < r ) {\n        long long mid = ( l + r ) / 2;\n        if( mid*mid > x ) r = mid - 1;\n        if( mid*mid < x ) l = mid + 1;\n        if( mid*mid == x ) l = r = mid;\n    }\n\n    return ( l*l == x ) ? l : -1;\n}" )
;;   (define-abbrev c++-mode-abbrev-table "dajmioper" "inline point operator + ( point A, point B ) { return point( A.X + B.X, A.Y + B.Y ); }\ninline point operator - ( point A, point B ) { return point( A.X - B.X, A.Y - B.Y ); }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmipoint" "typedef pair< int, int > point;\n#define X first\n#define Y second" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiccw" "inline int ccw( point a, point b, point c ) { return a.X * ( b.Y - c.Y ) + b.X * ( c.Y - a.Y ) + c.X * ( a.Y - b.Y ); }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiresize" "template< typename T > void resize( T &V, int n, int s ) { V.clear(); V.resize( n ); for( int i = 0; i < n; ++i ) V[i].resize( s ); }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmidist" "double dist( point a, point b ) { int ax = a.X - b.X; int ay = a.Y - b.Y; return sqrt( double( ax * ax + ay * ay ) ); }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmitri" "typedef pair< int, pair< int, int > > TRI;\n#define X first\n#define Y second.first\n#define Z second.second" )
;;   (define-abbrev c++-mode-abbrev-table "dajmigcd" "int gcd( int a, int b ) { return a && b ? gcd( b, a%b ) : a|b; }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiisprime" "bool is_prime( long long x )\n{\n    if( x <= 1 ) return false;\n\n    for( long long i = 2; i*i <= x; ++i )\n        if( x%i == 0 )\n            return false;\n\n    return true;\n}" )
;;   (define-abbrev c++-mode-abbrev-table "dajmilastbit" "inline int lastbit( int x ) { return (x^(x-1))&x; }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmitrace" "#define TRACE(s) cout << #s << \" = \" << (s) << endl" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiforc" "#define FORC(it,v) for( __typeof((v).begin()) it = (v).begin(); it != (v).end(); ++it )" )
;;   (define-abbrev c++-mode-abbrev-table "dajmimodul" "inline int modul( int a, int b ) { return (a%b+b)%b; }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiinf" "const int inf = 2147483647;" )
;;   (define-abbrev c++-mode-abbrev-table "dajmipi" "const double PI_C = 3.14159265358979323846264338327950288;" )
;;   (define-abbrev c++-mode-abbrev-table "biografija" "/************************************************\ \n  @author Goran Zuzic ( goran.zuzic@zg.t-com.hr ) \n\************************************************/" )
;;   (define-abbrev c++-mode-abbrev-table "komentar1" "// Cry as challange phase is passing by" )
;;   (define-abbrev c++-mode-abbrev-table "komentar2" "// O Milko najdraza preveslala me desetinka\n// da nije desetinke bilo bi tridesetice" )
;;   (define-abbrev c++-mode-abbrev-table "komentar3" "// Why do you even bother trying" )
;;   (define-abbrev c++-mode-abbrev-table "dajmisqr" "#define SQR(x) ((x)*(x))" )
;;   (define-abbrev c++-mode-abbrev-table "dajmipowermod" "" 'zuza-modpower )
;;   (define-abbrev c++-mode-abbrev-table "dajmipower" "" 'zuza-power )
;;   (define-abbrev c++-mode-abbrev-table "dajmivalid" "" 'zuza-valid )
;;   (define-abbrev c++-mode-abbrev-table "dajmimain" "" 'zuza-main )
;;   (define-abbrev c++-mode-abbrev-table "dajmieps" "const double EPS = 1e-9;" )
;;   (define-abbrev c++-mode-abbrev-table "dajmibuffer" "const int MAXLEN = 1000;\nchar buffer[MAXLEN];" )
;;   (define-abbrev c++-mode-abbrev-table "dajmitrim" "inline string trim( string s ) {\n    int l = s.find_first_not_of( \" \" );\n    int r = s.find_last_not_of( \" \" );\n    return string( s.begin() + l + ( l == -1 ), s.begin() + r + 1 );\n}" )
;;   (define-abbrev c++-mode-abbrev-table "dajmimyabs" "template< typename T > inline T myabs( const T &x ) { if( x < 0 ) return -x; return x; }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmieq" "inline bool eq( double a, double b ) { return myabs( a-b ) < EPS; }" )
;;   (define-abbrev c++-mode-abbrev-table "dajmiitos" "string itos( int x ) { ostringstream out; out << x; return out.str(); }" )
;;   (define-abbrev c++-mode-abbrev-table "dajminfactors" "long long n_factors( long long x )\n{\n    long long sum = 1;\n    for( int i = 2; i*i <= x; ++i ) {\n        int cnt = 1; for( cnt = 1; x%i == 0; x /= i ) ++cnt;\n        sum *= cnt;\n    }\n     \n    if( x > 1 ) sum *= 2;\n    return sum;\n }" )
;;   (define-abbrev c++-mode-abbrev-table "scanint" "" 'zuza-scanint )
;;   (define-abbrev c++-mode-abbrev-table "uniq" "" 'zuza-unique )
;;   (define-abbrev c++-mode-abbrev-table "forn" "" 'skel-forn)
;;   (define-abbrev c++-mode-abbrev-table "fr1" "" 'luka-for1 )
;;   (define-abbrev c++-mode-abbrev-table "fr2" "" 'luka-for2 )
;;   (define-abbrev c++-mode-abbrev-table "fr3" "" 'luka-for3 )
;;   (define-abbrev c++-mode-abbrev-table "fr" "" 'luka-for3 )
;;   (define-abbrev c++-mode-abbrev-table "lthen" "" 'zuza-lthen )
;;   (define-abbrev c++-mode-abbrev-table "lte" "" 'zuza-lte )
;;   (define-abbrev c++-mode-abbrev-table "gthen" "" 'zuza-gthen )
;;   (define-abbrev c++-mode-abbrev-table "gte" "" 'zuza-gte )
;;   (define-abbrev c++-mode-abbrev-table "forall" "" 'zuza-forall )
;;   (define-abbrev c++-mode-abbrev-table "forit2" "" 'luka-forit2 )
;;   (define-abbrev c++-mode-abbrev-table "forit3" "" 'luka-forit3 )
;;   (define-abbrev c++-mode-abbrev-table "forit" "" 'luka-forit2 )
;;   (define-abbrev c++-mode-abbrev-table "opmanje" "" 'luka-manje )
;;   (define-abbrev c++-mode-abbrev-table "srt" "" 'luka-sort )
;;   (define-abbrev c++-mode-abbrev-table "rev" "" 'luka-reverse )
;;   (define-abbrev c++-mode-abbrev-table "permute" "" 'luka-permute )
;;   (define-abbrev c++-mode-abbrev-table "setunion" "" 'luka-set-union )
;;   (define-abbrev c++-mode-abbrev-table "setinter" "" 'luka-set-intersection )
;;   (define-abbrev c++-mode-abbrev-table "xy" "" 'zuza-refxy )
;;   (define-abbrev c++-mode-abbrev-table "bins" "" 'zuza-bins )
;;   (define-abbrev c++-mode-abbrev-table "sins" "" 'zuza-sins )
;;   (define-abbrev c++-mode-abbrev-table "memnull" "" 'luka-memnull )
;;   (define-abbrev c++-mode-abbrev-table "meminf" "" 'luka-meminf )
;;   (define-abbrev c++-mode-abbrev-table "memminus" "" 'zuza-memminus )
;;   (define-abbrev c++-mode-abbrev-table "uint" "unsigned int" )
;;   (define-abbrev c++-mode-abbrev-table "ll" "long long" )
;;   (define-abbrev c++-mode-abbrev-table "vs" "vector< string >" )
;;   (define-abbrev c++-mode-abbrev-table "vi" "vector< int >" )
;;   (define-abbrev c++-mode-abbrev-table "vvi" "vector< vector< int > >" )
;;   (define-abbrev c++-mode-abbrev-table "vll" "vector< long long >" )
;;   (define-abbrev c++-mode-abbrev-table "vvll" "vector< vector< long long > >" )
;;   (define-abbrev c++-mode-abbrev-table "iss" "istringstream" )
;;   (define-abbrev c++-mode-abbrev-table "oss" "ostringstream" )
;;   (define-abbrev c++-mode-abbrev-table "delta4" "const int dx[4] = { -1, 0, 1, 0 };\nconst int dy[4] = { 0, 1, 0, -1 };" )
;;   (define-abbrev c++-mode-abbrev-table "delta8" "const int dx[8] = { -1, -1, 0, 1, 1, 1, 0, -1 };\nconst int dy[8] = { 0, 1, 1, 1, 0, -1, -1, -1 };" )
;;   (define-abbrev c++-mode-abbrev-table "konj8" "const int dx[8] = { 2, 2, -2, -2, 1, 1, -1, -1 };\nconst int dy[8] = { 1, -1, 1, -1, 2, -2, 2, -2 };" )
;;   (define-key c-mode-base-map (kbd "C-c c") 'comment-region)
;;   (define-key c-mode-base-map (kbd "C-c u") 'uncomment-region)
;;   (define-key c-mode-base-map (kbd "C-c r") 'query-replace)
;;   (define-key c-mode-base-map (kbd "C-c e") 'query-replace-regexp)
;;   (define-key c-mode-base-map (kbd "C-c l") 
;; 	(function (lambda () (interactive) (load-file "~/.emacs"))))
;;   (define-key c-mode-base-map "\M-p" 'previous-error)
;;   (define-key c-mode-base-map "\M-n" 'next-error)
;;   (define-key c-mode-base-map "`"   'smart-tab)
;;   (define-key c-mode-base-map (kbd "<C-tab>") 'c-indent-command)
;; )
;; (global-set-key (kbd "C-c d") (function (lambda () (interactive) (shell-command "start cmd &"))))
;; (global-set-key (kbd "C-x C-b") 'buffer-menu)

;; (add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(dabbrev-case-fold-search nil)
;;  '(dabbrev-case-replace nil))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(font-lock-builtin-face ((((class color) (background light)) (:foreground "magenta"))))
;;  '(font-lock-comment-face ((((class color) (background light)) (:foreground "forest green"))))
;;  '(font-lock-keyword-face ((((class color) (background light)) (:foreground "blue"))))
;;  '(font-lock-string-face ((((class color) (background light)) (:foreground "dim gray")))))

(require 'cl)

(defun zuza-close-tmp-buffers ()
"Close all visible windows that are temporary, ie. whose names start with an *"
 (interactive)
  (loop for w in
	(let ((kill-list nil))
	  (loop for w in (window-list) do 
		(let (( name (buffer-name (window-buffer w)) ))
		  (when (string-prefix-p "*" name)
		    (when (not (string= name "*scratch*"))
		      (setq kill-list (cons w kill-list))
		      )
		    )
		  )
		)
	  kill-list
	  )
	do (delete-window w)
 )
)

;; Graphviz
(global-set-key (kbd "M-2") 'compile)
(global-set-key (kbd "M-4") 'zuza-close-tmp-buffers)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

(load "graphviz-dot-mode")

(add-hook 'graphviz-dot-mode-hook
      (lambda () (local-set-key (kbd "M-3") 'graphviz-dot-preview))
)

;; async-eval (experiment)
(require 'async-eval)

;; CEDET
(load-file "~/site-lisp/cedet-1.0/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

(require 'semantic-ia)
(require 'semantic-gcc)

(load "etags")

(defun zuza-semantic-ia-fast-jump () (interactive)
	(ring-insert find-tag-marker-ring (point-marker))
	(semantic-ia-fast-jump (point))
)

(defun kill-all-local-variables-int () (interactive)
	(kill-all-local-variables)
)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
	(local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle);; ?
	(local-set-key "\C-cn" 'eassist-switch-h-cpp)
	(local-set-key "\M-," 'zuza-semantic-ia-fast-jump)
)
(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; integration with imenu -> set by default with code-helpers
;;(defun my-semantic-hook ()
;;  (imenu-add-to-menubar "TAGS"))
;;(add-hook 'semantic-init-hooks 'my-semantic-hook)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((compile-suffix . "-lboost_date_time -lboost_filesystem -lglog -lgflags") (compile-suffix . "-lboost_date_time -lboost_filesystem -lboost_thread") (compile-suffix . "-lboost_date_time -lboost_filesystem"))))
 '(save-abbrevs nil))

; ECB

(add-to-list 'load-path "/home/gzuzic/site-lisp/ecb-2.40/")
(require 'ecb)

; cedet config
; (require 'semantic-gcc)

; My projects

(ede-cpp-root-project "boost_projekt"
 	        :name "Moj boost projekt"
                :file "/home/gzuzic/Downloads/boost_1_47_0/INSTALL"
                :include-path '("/"
                               )
                :system-include-path '("/usr/include"
                                      "/usr/local/include"
                                       )
)

(ede-cpp-root-project "boost_fajlovi"
 	        :name "Moji fajlici"
                :file "/home/gzuzic/work/boost/mpl.cpp"
                :include-path '("/home/gzuzic/Downloads/boost_1_47_0/"
                               )
)
