;;; laws.el --- -*- Coding: utf-8 -*-

;; Copyright (C) 2007, 2008  Kazushi NODA

;; Author: Kazushi NODA (http://www.ne.jp/asahi/alpha/kazu/)
;; Created: 2007-10-31
;; Version: $Id: laws.el,v 1.110 2008/04/26 13:49:34 kazu Exp $
;; Keywords:

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'revive)
(require 'easymenu)
(require 'outline)
(require 'poem)
(require 'iswitchb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; laws-vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defconst laws-version "version 0.8.7"
  "Version of laws.el")

;;
;; Customize group
;;
(defgroup laws nil
  "Laws mode."
  :prefix "laws-"
  :group 'applications)

(defgroup laws-faces nil
  "Faces for highlighting text."
  :prefix "laws-"
  :group 'faces)

;;
;; User variables
;;
(defcustom laws-line-space nil
  "*法令ファイルの行間。"
  :type 'integer
  :group 'laws)

(defcustom laws-index-initial-mode 'Index
  "`laws-index-mode'の初期モード。次のうちひとつ選択できる。
Opened Recent Search Bookmark Index Directory Abbrev"
  :group 'laws
  :type '(choice
	  (symbol :tag "Default" :value Index)
	  (symbol :tag "Opened" :value Opened)
	  (symbol :tag "Recent" :value Recent)
	  (symbol :tag "Search" :value Search)
	  (symbol :tag "Bookmark" :value Bookmark)
	  (symbol :tag "Index" :value Index)
	  (symbol :tag "Directory" :value Directory)
	  (symbol :tag "Abbrev" :value Abbrev)))

(defcustom laws-recent-max 100
  "最近開いたファイルの履歴のリスト`laws-recent-alist'の最大数。"
  :type 'integer
  :group 'laws)

(defcustom laws-use-index-header-line t
  "ヘッダラインを表示する。"
  :type 'boolean
  :group 'laws)

(defcustom laws-coding-system-for-write 'euc-jp-unix
  "法令ファイルの文字コード。"
  :type 'symbol
  :group 'laws)

(defcustom laws-window-height 15
  "参照条文を表示するウィンドウの高さ。"
  :type 'integer
  :group 'laws)

(defcustom laws-reposition-line 2
  ""
  :type 'integer
  :group 'laws)

(defcustom laws-anchor-clickable nil
  "法令内のアンカーをクリッカブルにする。"
  :type 'boolean
  :group 'laws)

(defcustom laws-use-iswitchb nil
  "`laws-index-goto-folder'で`iswitchb'を使う。migemoとiswitchbの設定が必要。"
  :type 'boolean
  :group 'laws)

(defcustom laws-iswitchb-initial-list 'download
  "`laws-iswitchb'での検索対象の法令リストの初期値。"
  :group 'laws
  :type '(choice
	  (symbol :tag "download" :value download)
	  (symbol :tag "bookmark" :value bookmark)
	  (symbol :tag "all" :value all)))

(defcustom laws-max-mini-window-height max-mini-window-height
  "`laws-iswitchb'のミニウインドウの最大高さ。"
  :type 'number
  :group 'laws)

(defcustom laws-online-mode t
  "オンラインモードで起動する。"
  :type 'boolean
  :group 'laws)

(defcustom laws-excluded-law-names
  '("社会保険労務士法"
    "土地家屋調査士法"
    "行政書士法"
    "司法書士法"
    "税理士法"
    "弁護士法"
    "調査士法")
  "法令名と法人名が重複する法令名のリスト。法の文言の中に現われる
 「〜法人」を着色しない。"
  :type 'list
  :group 'laws)

(defcustom laws-unentry-names
  '(("憲法" . "日本国憲法"))
  "法の文言の中で使われるが、略称法令名にも登録法令名にも含まれな
い法令名のアリスト。"
  :type 'alist
  :group 'laws)

(defcustom laws-local-names-plist
  '(("不動産登記令" :法 "不動産登記法" :規則 "不動産登記規則"))
  "ある法令ファイルの中での「法」・「規則」・「令」などを指し示す法令名。
先頭の要素が法令ファイルの法令名。上の例では、「不動産登記令」の中で、
 「法」は「不動産登記法」を指し、「規則」は「不動産登記規則」を指す。
法文の中で、`（以下「法」という。）'のように記述されているので、
ここで設定せずとも対応可能だが、設定されていれば優先して適用される。
タグは`laws-local-name-list'に登録されている名称を利用する。"
  :type 'list
  :group 'laws)

;; Path names
(defcustom laws-path (expand-file-name "~/.laws.d")
  "法令データ提供システムから取得したインデックスファイル、法令デー
タ等の保存先パス。"
  :type 'directory
  :group 'laws)

(defcustom laws-htmldata-directory "htmldata"
  "法令データ適用システムからダウンロードしたhtmldataの保存先ディレクトリ
の親ディレクトリ名。"
  :type 'string
  :group 'laws)

(defcustom laws-htmldata-path (concat laws-path "/" laws-htmldata-directory)
  "法令データ適用システムからダウンロードしたhtmldataの保存先ディレクトリ
の親ディレクトリのパス名。"
  :type 'directory
  :group 'laws)

(defcustom laws-egov-url "http://law.e-gov.go.jp/"
  "法令データ提供システムのURL。"
  :type 'directory
  :group 'laws)

(defcustom laws-data-path (concat laws-path "/data")
  "w3mでdumpした法令ファイルの保存先ディレクトリの親ディレクトリのパス名。"
  :type 'directory
  :group 'laws)

(defcustom laws-temp-path (concat laws-path "/tmp")
  "w3mでdumpする一時ファイルの保存先ディレクトリのパス名。"
  :type 'directory
  :group 'laws)

(defcustom laws-extention ".law"
  "法令データファイルの拡張子。`auto-mode-alist'に追加される。"
  :type 'string
  :group 'laws)

(defcustom laws-index-file (concat laws-path "/.index")
  "事項別インデックスファイルのファイル名。"
  :type 'file
  :group 'laws)

(defcustom laws-abbrev-file (concat laws-path "/.abbrev")
  "略称法令名のインデックスファイルのファイル名。"
  :type 'file
  :group 'laws)

(defcustom laws-bookmark-file (concat laws-path "/.bookmark")
  "ブックマークの保存先ファイル名。"
  :type 'file
  :group 'laws)

(defcustom laws-recent-file (concat laws-path "/.recent")
  "最近開いたファイルのリストの保存先ファイル名"
  :type 'file
  :group 'laws)

;; Buffer name
(defcustom laws-use-buffer-law-name t
  "tならバッファ名を法令名とする。nilなら変更しない。"
  :type 'boolean
  :group 'laws)

(defcustom laws-name-suffix ".."
  "バッファ名を縮小する場合のSUFFIX。"
  :type 'string
  :group 'laws)

(defcustom laws-name-length 20
  "バッファ名(法令名)の最大長。"
  :type 'integer
  :group 'laws)

;; w3m
(defcustom laws-w3m-dump-cols 5000
  "w3mでdumpするときのカラム数。"
  :type 'integer
  :group 'laws)

(defcustom laws-table-pixel 550
  "w3mでdumpするときのテーブルのピクセル数。"
  :type 'integer
  :group 'laws)

;; paren
(defcustom laws-compose-paren-char "＃"
  "`compose-region'で括弧を不可視にする場合、代替の1文字。
ここで指定する文字が対応括弧の代わりる表示される。"
  :type 'string
  :group 'laws)

;; 
;; font-lock-keyword-face
;; 
;; 以下、defcustomされている`laws-*-face'の値(シンボル名)を他に変更しないこと。
(defvar laws-index-flag-face 'laws-index-flag-face
  "Face name to use for open or close flag of laws index mode.")

(defvar laws-index-header-key-face 'laws-index-header-key-face
  "Face name to use for shortcut key of header line.")

(defvar laws-index-header-foreground-face 'laws-index-header-foreground-face
  "Face name to use for foreground of header line.")

(defvar laws-index-header-selected-face 'laws-index-header-selected-face
  "Face name to use for selected foreground of header line.")

(defvar laws-article-number-face 'laws-article-number-face
  "Face name to use for number of article.")

(defvar laws-article-paragraph-face 'laws-article-paragraph-face
  "Face name to use for number of article.")

(defvar laws-article-item-face 'laws-article-item-face
  "Face name to use for number of article.")

(defvar laws-anchor-name-face 'laws-anchor-name-face
  "Face name to use for laws name.")

(defvar laws-anchor-article-face 'laws-anchor-article-face
  "Face name to use for anchor article.")

(defvar laws-anchor-paragraph-face 'laws-anchor-paragraph-face
  "Face name to use for number of terms.")

(defvar laws-article-subnumber-face 'laws-article-subnumber-face
  "Face name to use for articl sub number.")

(defvar laws-article-subitem1-face 'laws-article-subitem1-face
  "Face name to use for article sub item.")

(defvar laws-article-subitem2-face 'laws-article-subitem2-face
  "Face name to use for article sub item-2.")

(defvar laws-article-subitem3-face 'laws-article-subitem3-face
  "Face name to use for article sub item-3.")

(defvar laws-article-subitem4-face 'laws-article-subitem4-face
  "Face name to use for article sub item-4.")

(defvar laws-volume-face 'laws-volume-face
  "Face name to use for volume.")

(defvar laws-chapter-face 'laws-chapter-face
  "Face name to use for chapter.")

(defvar laws-section-face 'laws-section-face
  "Face name to use for section.")

(defvar laws-subsection-face 'laws-subsection-face
  "Face name to use for subsection.")

(defvar laws-subsection2-face 'laws-subsection2-face
  "Face name to use for subsection-2.")

(defvar laws-comment-face 'laws-comment-face
  "Face name to use for comment.")

(defvar laws-supplementary-face 'laws-supplementary-face
  "Face name to use for supplementary.")

(defvar laws-paren1-face 'laws-paren1-face
  "Face name to use for Parentheses of the 1'th hierarchy.")

(defvar laws-paren2-face 'laws-paren2-face
  "Face name to use for Parentheses of the 2'th hierarchy.")

(defvar laws-paren3-face 'laws-paren3-face
  "Face name to use for Parentheses of the 3'th hierarchy.")

(defvar laws-paren4-face 'laws-paren4-face
  "Face name tp use for Parentheses of the 4'th hierarchy.")

(defvar laws-paren5-face 'laws-paren5-face
  "Face name tp use for Parentheses of the 5'th hierarchy.")

(defvar laws-paren6-face 'laws-paren6-face
  "Face name to use for Parentheses of the 6'th hierarchy.")

(defvar laws-paren-error-face 'laws-paren-error-face
  "Face name to use for Parentheses of the max hierarchy.")

;; 
;; faces
;; 
(defface laws-index-flag-face
    '((((class color) (background light))
       (:foreground "CadetBlue"))
      (((class color) (background dark))
       (:foreground "Aquamarine"))
      (t (:foreground "SkyBlue")))
  "Font Lock mode face used to highlight laws index folder flag."
  :group 'laws-faces)

(defface laws-index-header-key-face
    '((((class color) (background light))
       (:foreground "Black" :background nil :underline nil :weight bold))
      (((class color) (background dark))
       (:foreground "White" :background nil :underline nil :weight bold))
      (t (:foreground nil :underline t)))
  "Font Lock mode face used to highlight laws index header line."
  :group 'laws-faces)

(defface laws-index-header-selected-face
    '((((class color) (background light))
       (:foreground "Black" :background nil :underline nil :weight bold
	:box (:line-width -1 :color nil :style pressed-button)))
      (((class color) (background dark))
       (:foreground "White" :background nil :underline nil :weight bold
	:box (:line-width -1 :color nil :style pressed-button)))
      (t (:foreground nil :underline t :weight bold)))
  "Font Lock mode face used to highlight laws index header line."
  :group 'laws-faces)

(defface laws-index-header-foreground-face
    '((((class color) (background light))
       (:foreground "Gray60" :background nil :weight bold))
      (((class color) (background dark))
       (:foreground "CornflowerBlue" :background nil :weight bold))
      (t (:foreground nil :weight bold)))
  "Font Lock mode face used to highlight laws index header line."
  :group 'laws-faces)

(defface laws-volume-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "Medium aquamarine"))
      (t (:foreground "Medium aquamarine")))
  "Font Lock mode face used to highlight volume lines."
  :group 'laws-faces)

(defface laws-chapter-face
    '((((class color) (background light))
       (:foreground "DarkGoldenrod"))
      (((class color) (background dark))
       (:foreground "Green3"))
      (t (:foreground "Green3")))
  "Font Lock mode face used to highlight chapter lines."
  :group 'laws-faces)

(defface laws-section-face
    '((((class color) (background light))
       (:foreground "Purple"))
      (((class color) (background dark))
       (:foreground "Cyan"))
      (t (:foreground "Cyan")))
  "Font Lock mode face used to highlight section lines."
  :group 'laws-faces)

(defface laws-subsection-face
    '((((class color) (background light))
       (:foreground "Orchid"))
      (((class color) (background dark))
       (:foreground "LightSteelBlue"))
      (t (:foreground "LightSteelBlue")))
  "Font Lock mode face used to highlight subsection lines."
  :group 'laws-faces)

(defface laws-subsection2-face
    '((((class color) (background light))
       (:foreground "red"))
      (((class color) (background dark))
       (:foreground "Dark sea green"))
      (t (:foreground "Dark sea green")))
  "Font Lock mode face used to highlight subsection-2."
  :group 'laws-faces)

(defface laws-comment-face
    '((((class color) (background light))
       (:foreground "CadetBlue"))
      (((class color) (background dark))
       (:foreground "LightSteelBlue"))
      (t (:foreground "LightSteelBlue")))
  "Font Lock mode face used to highlight comment."
  :group 'laws-faces)

(defface laws-anchor-article-face
    '((((class color) (background light))
       (:foreground nil :underline t))
      (((class color) (background dark))
       (:foreground nil :underline t))
      (t (:foreground nil :underline t)))
  "Font Lock mode face used to highlight reference."
  :group 'laws-faces)

(defface laws-anchor-name-face
    '((((class color) (background light))
       (:foreground nil :underline t))
      (((class color) (background dark))
       (:foreground nil :underline t))
      (t (:foreground nil :underline t)))
  "Font Lock mode face used to highlight laws name."
  :group 'laws-faces)

(defface laws-supplementary-face
    '((((class color) (background light))
       (:foreground "CadetBlue"))
      (((class color) (background dark))
       (:foreground "Brown3"))
      (t (:foreground "Brown3")))
  "Font Lock mode face used to highlight comment."
  :group 'laws-faces)

(defface laws-paren1-face
    '((((class color) (background light))
       (:foreground "Palevioletred3"))
      (((class color) (background dark))
       (:foreground "Palevioletred3"))
      (t (:foreground "Palevioletred3")))
  "Parentheses of the 1'th hierarchy."
  :group 'laws-faces)

(defface laws-paren2-face
    '((((class color) (background light))
       (:foreground "Brown"))
      (((class color) (background dark))
       (:foreground "Brown"))
      (t (:foreground "Brown")))
  "Parentheses of the 2'th hierarchy."
  :group 'laws-faces)

(defface laws-paren3-face
    '((((class color) (background light))
       (:foreground "Yellow4"))
      (((class color) (background dark))
       (:foreground "Yellow4"))
      (t (:foreground "Yellow4")))
  "Parentheses of the 3'th hierarchy."
  :group 'laws-faces)

(defface laws-paren4-face
    '((((class color) (background light))
       (:foreground "Tan3"))
      (((class color) (background dark))
       (:foreground "Tan3"))
      (t (:foreground "Tan3")))
  "Parentheses of the 4'th hierarchy."
  :group 'laws-faces)

(defface laws-paren5-face
    '((((class color) (background light))
       (:foreground "RosyBrown3"))
      (((class color) (background dark))
       (:foreground "RosyBrown3"))
      (t (:foreground "RosyBrown3")))
  "Parentheses of the 5'th hierarchy."
  :group 'laws-faces)

(defface laws-paren6-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "Blue"))
      (t (:foreground "Blue")))
  "Parentheses of the 6'th hierarchy."
  :group 'laws-faces)

(defface laws-article-number-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "LightSkyBlue"))
      (t (:foreground "LightSkyBlue")))
  "Font Lock mode face used to highlight article number."
  :group 'laws-faces)

(defface laws-article-paragraph-face
    '((((class color) (background light))
       (:foreground "DarkGreen"))
      (((class color) (background dark))
       (:foreground "Cyan"))
      (t (:foreground "Cyan")))
  "Font Lock mode face used to highlight paragraph number."
  :group 'laws-faces)

(defface laws-article-item-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "Red"))
      (t (:foreground "Red")))
  "Font Lock mode face used to highlight item number."
  :group 'laws-faces)

(defface laws-anchor-paragraph-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "LightSkyBlue"))
      (t (:foreground "LightSkyBlue")))
  "Font Lock mode face used to highlight number of termss."
  :group 'laws-faces)

(defface laws-article-subnumber-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "IndianRed1"))
      (t (:foreground "IndianRed1")))
  "Font Lock mode face used to highlight article sub number."
  :group 'laws-faces)

(defface laws-article-subitem1-face
    '((((class color) (background light))
       (:foreground "Blue"))
      (((class color) (background dark))
       (:foreground "Green"))
      (t (:foreground "Green")))
  "Font Lock mode face used to highlight article sub item."
  :group 'laws-faces)

(defface laws-article-subitem2-face
    '((((class color) (background light))
       (:foreground "Orange"))
      (((class color) (background dark))
       (:foreground "Orange"))
      (t (:foreground "Orange")))
  "Font Lock mode face used to highlight article sub item-2."
  :group 'laws-faces)

(defface laws-article-subitem3-face
    '((((class color) (background light))
       (:foreground "Red"))
      (((class color) (background dark))
       (:foreground "HotPink1"))
      (t (:foreground "HotPink1":weight bold)))
  "Font Lock mode face used to highlight article sub item-3."
  :group 'laws-faces)

(defface laws-article-subitem4-face
    '((((class color) (background light))
       (:foreground "Maroon"))
      (((class color) (background dark))
       (:foreground "Pink4"))
      (t (:foreground "Pink4")))
  "Font Lock mode face used to highlight article sub item-4."
  :group 'laws-faces)

;;
;; Internal variables
;;

;; Mode line image
(defvar laws-online-icon
  (or (plist-get (cdr (find-image '((:type xpm :file "ezimage/doc-plus.xpm"))))
		 :file)
      "[+]")
  "Mode line online image path.")

(defvar laws-offline-icon
  (or (plist-get (cdr (find-image '((:type xpm :file "ezimage/doc-minus.xpm"))))
		 :file)
      "[-]")
  "Mode line offline image path.")

(defvar laws-icon
  (if laws-online-mode laws-online-icon laws-offline-icon)
  "Start up mode line image path.")

(defvar laws-mode-line-buffer-identification
  (default-value 'mode-line-buffer-identification))
(make-variable-buffer-local 'laws-mode-line-buffer-identification)

(defvar laws-mode-line
  (cons `(:eval (propertize
		 ,@(if (and (file-exists-p laws-online-icon)
			    (file-exists-p laws-offline-icon))
		       (list
			"    "
			(quote 'display) '(create-image
					   laws-icon 'xpm nil
					   :ascent 'center))
		       (list 'laws-icon))
		 'local-map (make-mode-line-mouse-map
			     'mouse-1 'laws-online-or-offline)
		 'mouse-face 'mode-line-highlight
		 'help-echo (if laws-online-mode
				"mouse-1: turn to offline mode."
			      "mouse-1: turn to online mode.")))
	(cons " "
	      (list (car laws-mode-line-buffer-identification))))
  "Laws mode line format.")

;; BookMark

;; Search
(defvar laws-search-history nil)

;; Winconf
(defvar laws-winconf-list '())

(defvar laws-winconf-index 0)

(defvar laws-display-toggle-winconf nil
  "For laws-display-toggle.")

;; Parentheses
(defvar laws-parens "（.+）")

(defvar laws-paren-exclude-regexp
  "（\\([０-９]+\\|[一二三四五六七八九十]+\\|[ｉ]+\\)）")

(defvar laws-paren-overlays)

;; 法、令、規則、新法、旧法等への対応
(defvar laws-local-name-list
  '("法" "新法" "旧法" "規則" "新規則" "旧規則" "令" "新令" "旧令" "新細則" "旧細則" "附則" "法附則" "規則附則")
  "")

;; outline
(defvar laws-heading-regexp
  (let ((number "[一二三四五六七八九十]"))
    (concat "^　*"
	    "\\(第" number "+編"
	    "\\|第" number "+章\\([のノ]" number "\\)*"
	    "\\|第" number "+節\\([のノ]" number "\\)*"
	    "\\|第" number "+款\\([のノ]" number "\\)*"
	    "\\|第" number "+目\\([のノ]" number "\\)*"
	    "\\|附　?則\\)"))
  "\
非アウトラインヘッダを含む見出しの正規表現。
目次部分の非アウトラインヘッダの見出しが行頭から始まる場合には、アウト
ラインヘッダ`outline-regexp'でもあるので、見出-本文間の移動が機能しない。
そのため、少なくとも1個の全角空白が必要。これは手作業で挿入する必要が
ある。")

(defvar laws-supplementary-level 6
  "附則のアウトラインレベル")

;; laws-mode
(defvar laws-mishikou-list)		;ローカル変数

(defvar laws-mode-name "Laws")

(defvar laws-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;; laws-anchor-clickable
    (define-key map [mouse-2] 'laws-push-mouse-2)
    (define-key map [follow-link] 'mouse-face)
    ;; quit
    (define-key map "q" 'laws-view-quit)
    (define-key map "Q" 'laws-exit)
    ;; search
    (dotimes (i 10)
      (define-key map (format "%d" i) 'digit-argument))
    (define-key map "-" 'laws-digit-argument-suffix)
    (define-key map " " 'laws-search-or-push-anchor)
    ;; bookmark
    (define-key map "A" 'laws-bookmark-this-file)
    ;; scroll
    (define-key map "n" 'laws-forward-article)
    (define-key map "p" 'laws-backward-article)
    (define-key map "[" 'laws-backward-paragraph)
    (define-key map "]" 'laws-forward-paragraph)
    (define-key map "j" 'laws-scroll-up-screen)
    (define-key map "k" 'laws-scroll-down-screen)
    ;; anchor
    (define-key map "\C-i" 'laws-forward-anchor)
    (define-key map "\C-\M-i" 'laws-backward-anchor)
    ;; window
    (define-key map "v" 'laws-display-toggle)
    (define-key map "o" 'laws-other-window)
    ;; winconf
    (define-key map "wp" 'laws-backward-winconf)
    (define-key map "wn" 'laws-forward-winconf)
    (define-key map "wi" 'laws-winconf-insert)
    (define-key map "wa" 'laws-winconf-add)
    (define-key map "wo" 'laws-winconf-override)
    (define-key map "wd" 'laws-winconf-delete)
    (define-key map "wD" 'laws-winconf-delete-all)
    (define-key map "wh" 'laws-winconf-backward-delete)
    (define-key map "wf" 'laws-restore-first-winconf)
    (define-key map "wl" 'laws-restore-last-winconf)
    (define-key map "wc" 'laws-restore-current-winconf)
    (define-key map "," 'laws-backward-winconf)
    (define-key map "." 'laws-forward-winconf)
    (define-key map "<" 'laws-restore-first-winconf)
    (define-key map ">" 'laws-restore-last-winconf)
    (define-key map "wm" 'laws-winconf-message)
    ;; paren
    (define-key map "e" 'laws-fontify-or-defontify-paren)
    (define-key map "dd" 'laws-decompose-paren)
    (define-key map "de" 'laws-compose-paren)
    ;; outline
    (define-key map "gt" 'laws-goto-toc)
    (define-key map "t" 'laws-goto-toc)
    (define-key map "gu" 'laws-up-heading)
    (define-key map "u" 'laws-up-heading)
    (define-key map "gf" 'laws-next-visible-heading)
    (define-key map "f" 'laws-next-visible-heading)
    (define-key map "gb" 'laws-previous-visible-heading)
    (define-key map "b" 'laws-previous-visible-heading)
    (define-key map "gn" 'laws-forward-same-level)
    (define-key map "gp" 'laws-backward-same-level)
    (define-key map "gg" 'laws-heading-jump)
    (define-key map "gc" 'laws-move-to-tables)
    (define-key map "c" 'laws-move-to-tables)
    ;; iswitchb
;;    (define-key map "\C-c\C-b" 'laws-iswitchb)
    ;; help
    (define-key map "?" 'laws-help)
    map))

;; laws-index-mode
(defvar laws-index-mode-name "Laws"
  "`laws-index-mode'のモード名。")

(defvar laws-index-buffer "*Laws*"
  "`laws-index-mode'のバッファ名。")

(defvar laws-index-header-items
  '("Opened"
    "Recent"
    "Search"
    "Bookmark"
    "Index"
    "Directory"
    "Abbrev")
  "`laws-index-mode'のヘッダラインのモードを表わす項目。")

;; 個別のモードの状態を保存するグローバル変数。
(defvar laws-recent-alist nil)
(defvar laws-bookmark-alist nil)
(defvar laws-opened-alist nil)
(defvar laws-search-history nil)

(defvar laws-alist nil)
(defvar laws-abbrev nil)
(defvar laws-abbrev-alist nil)

;; 個別のモードの状態を保存するローカル変数。
(defvar laws-index-local-mode)
(defvar laws-index-conf)
(defvar laws-names-alist)
(defvar laws-search-alist)
(defvar laws-index-alist)
(defvar laws-directory-alist)
;; Searchモードでハイライトのためのoverlayを保持するローカル変数。
(defvar laws-index-search-overlaies)

;; iswitchb
(defvar laws-names-list nil "iswitchbのための補完リスト")

(defvar laws-iswitchb-present-list nil
  "`laws-iswitchb'の現在の検索対象の法令リスト。")

;; w3m
(defvar laws-w3m-dump-command
  (lambda (cols) (format "w3m -dump -cols %s" cols)))

;; laws-index
(defun laws-set-face-invisible (n)
  "不可視のプロパティを設定するフォームを返す。"
  `(,n (progn (put-text-property
	       (match-beginning ,n) (match-end ,n)
	       'invisible t)
	      nil)))

(defun laws-set-mouse-face-1 (n)
  `(,n (progn (add-text-properties
	       (match-beginning ,n) (match-end ,n)
	       (list
		'mouse-face 'highlight
		'local-map 'laws-index-mode-map))
	      nil)))

(defun laws-set-mouse-face-2 (n)
  `(,n (progn (add-text-properties
	       (match-beginning ,n) (match-end ,n)
	       (list
		'mouse-face 'highlight
		'local-map 'laws-mode-map))
	      nil)))

(defvar laws-index-font-lock-keywords
  (let ((fcolor (cdr (assq 'foreground-color
			   (frame-parameters (selected-frame))))))
    (list `(
	    ;;
	    ;; Directory, Index
	    ;;
	    ;; folder
	    "^\\((\"\\)\\([+-]\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\")\\)$"
	    ,(laws-set-face-invisible 1)
	    (2 laws-index-flag-face t)
	    ,(laws-set-mouse-face-1 2)
	    ,(laws-set-face-invisible 3)
	    ,(laws-set-face-invisible 4)
	    (5 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 5)
	    ,(laws-set-face-invisible 6))

	  ;;
	  ;; Abbrev
	  ;;
	  ;; folder
	  `("^\\((\"\\) \\{2\\}\\([+-]\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\")\\)"
	    ,(laws-set-face-invisible 1)
	    (2 laws-index-flag-face t)
	    ,(laws-set-mouse-face-1 2)
	    ,(laws-set-face-invisible 3)
	    ,(laws-set-face-invisible 4)
	    (5 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 5)
	    ,(laws-set-face-invisible 6))
	  ;; sub folder
	  `("^\\((\"\\) \\{4\\}\\([+-]\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\")\\)"
	    ,(laws-set-face-invisible 1)
	    (2 laws-index-flag-face t)
	    ,(laws-set-mouse-face-1 2)
	    ,(laws-set-face-invisible 3)
	    ,(laws-set-face-invisible 4)
	    (5 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 5)
	    ,(laws-set-face-invisible 6))
	  `("^\\((\"\\) \\{6\\}\\(-\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\")\\)"
	    ,(laws-set-face-invisible 1)
	    (2 laws-index-flag-face t)
	    ,(laws-set-mouse-face-1 2)
	    ,(laws-set-face-invisible 3)
	    ,(laws-set-face-invisible 4)
	    (5 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 5)
	    ,(laws-set-face-invisible 6))

	  ;;
	  ;; Opened, Recent, Bookmark
	  ;;
	  `("^\\((\"\\)\\([ D]\\)\\(-\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\".+)\\)$"
	    ,(laws-set-face-invisible 1)
	    (2 '(:foreground "red") t)
	    ,(laws-set-mouse-face-1 2)
	    (3 laws-index-flag-face t)
	    ,(laws-set-face-invisible 4)
	    ,(laws-set-face-invisible 5)
	    (6 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 6)
	    ,(laws-set-face-invisible 7))

	  ;;
	  ;; 法令名
	  ;;
	  `("^\\((\"\\) \\{2,\\}\\(-\\)\\(\"\\) \\(\"\\)\\([^\"]+\\)\\(\".+)\\)$"
	    ,(laws-set-face-invisible 1)
	    (2 laws-index-flag-face t)
	    ;;,(laws-set-mouse-face-1 2)
	    ,(laws-set-face-invisible 3)
	    ,(laws-set-face-invisible 4)
	    (5 '(:foreground ,fcolor) t)
	    ,(laws-set-mouse-face-1 5)
	    ,(laws-set-face-invisible 6))
	  ))
  "`laws-index-mode'のための`font-lock-keywords'")

(defvar laws-index-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (mode)
	    (define-key map (vector (downcase (aref mode 0)))
	      `(lambda () (interactive) (laws-index-goto-mode (intern ,mode)))))
	  laws-index-header-items)
    (define-key map [mouse-2] 'laws-index-mouse-open-or-close)
    (define-key map [follow-link] 'mouse-face)
    (define-key map " " 'laws-index-open-or-close)
    (define-key map "u" 'laws-index-upper-level)
    (define-key map "\M-[" 'laws-index-open-all)
    (define-key map "\M-]" 'laws-index-close-all)
    (define-key map "p" 'laws-index-previous-line)
    (define-key map "n" 'laws-index-next-line)
    (define-key map "j" 'laws-index-scroll-up-line)
    (define-key map "k" 'laws-index-scroll-down-line)
    (define-key map "\M-p" 'laws-index-previous-folder)
    (define-key map "\M-n" 'laws-index-next-folder)
    (define-key map "\M-<" 'laws-index-beginning-of-buffer)
    (define-key map "\M->" 'laws-index-end-of-buffer)
    (define-key map "\C-j" 'laws-index-goto-folder)
    (define-key map "A" 'laws-index-bookmark-add)
    (define-key map "m" 'laws-index-put-deletion-flag)
    (define-key map "x" 'laws-index-do-delete-marks)
    (define-key map "P" 'laws-index-bookmark-move-up)
    (define-key map "N" 'laws-index-bookmark-move-down)
    (define-key map "S" 'laws-index-search)
    (define-key map "g" 'laws-index-update)
    (define-key map "q" 'bury-buffer)
    (define-key map "Q" 'laws-exit)
;;    (define-key map "\C-c\C-b" 'laws-iswitchb)
    (define-key map "?" 'laws-index-help)
    map)
  "`laws-index-mode'のキーマップを返す。")

;; Regexp
(defvar laws-volume-face-regexp
  "\\(^[ 　]*第.+編　[^（）\n]*\\)")

(defvar laws-chapter-face-regexp "\
\\(^[ 　]*第.+章\\([のノ][一二三四五六七八九十]\\)*　[^（）\n]+\\)")

(defvar laws-section-face-regexp "\
\\(^[ 　]*第.+節\\([のノ][一二三四五六七八九十]\\)*　[^（）\n]+\\)")

(defvar laws-subsection-face-regexp "\
\\(^[ 　]*第.+款\\([のノ][一二三四五六七八九十]\\)*　[^（）\n]+\\)")

(defvar laws-subsection2-face-regexp "\
^\\([ 　]*第.+目\\([のノ][一二三四五六七八九十]\\)*　[^（）\n]+\\)")

(defvar laws-article-subitem3-face-regexp "^\\(（[０-９]+）\\|([0-9]+)\\)　")

(defvar laws-comment-face-regexp "^[ 　]*\\(（\\cj+）\\|(\\cj+) \\)$")

(defvar laws-article-subitem4-face-regexp "^（ｉ+）　")

(defvar laws-supplementary-face-regexp
  "^\\([　 ]*附　?則.*$\\)")

(defvar laws-article-subitem2-face-regexp "^\\cK　")

(defvar laws-article-number-face-regexp "\
\\(^第[一二三四五六七八九十百千]+条\\([ノの][一二三四五六七八九十百]+\\)*\\)[ 　]*")

(defvar laws-article-paragraph-face-regexp "\
\\(^\\([○◯]\\)?\\([０-９]+\\|[0-9]+\\)\\)[ 　]*")

(defvar laws-article-item-face-regexp "\
\\(^[一二三四五六七八九十]+\\([のノ][一二三四五六七八九十]+\\)*\\)[ 　]*")

(defvar laws-anchor-name-face-regexp2 "\
\\([^同]\\(\\([新旧]?\\(附則\\|法附則\\|規則附則\\|細則\\|法\\|規則\\|令\\)\\)\
\\(\\(第[一二三四五六七八九十百千]+条\\)\\([のノ][一二三四五六七八九十]+\\)*\
\\(第[一二三四五六七八九十]+項\\)*\
\\(第[一二三四五六七八九十]+号\\([のノ][一二三四五六七八九十]+\\)*\\)*\\)\\)\\)")

(defvar laws-anchor-article-face-regexp3 "\
.\\(\\(第[一二三四五六七八九十百千]+条\\)\\([のノ][一二三四五六七八九十]+\\)*\
\\(第[一二三四五六七八九十]+項\\)*\
\\(第[一二三四五六七八九十]+号\\([のノ][一二三四五六七八九十]+\\)*\\)*\\)")

(defvar laws-article-regexp
  "^第[一二三四五六七八九十百千]+条\\([のノ][一二三四五六七八九十]+\\)\\{0,3\\}"
  "条数の正規表現。")

(defvar laws-paragraph-regexp
  (concat "^[○◯]?\\(" laws-article-regexp "\\|[０-９]\\|[0-9]\\)\\{1,2\\}")
  "項数の正規表現。")

;; font-lock-keywords
(defvar laws-font-lock-keywords)
(defvar laws-font-lock-keywords-0
  (list `(,(concat "\\(" (regexp-opt laws-excluded-law-names) "\\)[^人]")
	   (1 laws-anchor-name-face nil))))

(defvar laws-font-lock-keywords-1
  (list `(,laws-chapter-face-regexp 1 laws-chapter-face)
	`(,laws-section-face-regexp 1 laws-section-face)
	`(,laws-subsection-face-regexp 1 laws-subsection-face)
	`(,laws-subsection2-face-regexp 1 laws-subsection2-face)
	`(,laws-supplementary-face-regexp 1 laws-supplementary-face)
	`(,laws-article-number-face-regexp 1 laws-article-number-face)
	`(,laws-anchor-name-face-regexp2 3 laws-anchor-name-face)
	`(,laws-anchor-name-face-regexp2 4 laws-anchor-name-face)
	'("同法" 0 laws-anchor-name-face)
	`(,laws-anchor-article-face-regexp3 1 laws-anchor-article-face)
	`(,laws-anchor-article-face-regexp3 ,(laws-set-mouse-face-2 1))
	`("同法" ,(laws-set-mouse-face-2 0))
	`(,laws-article-paragraph-face-regexp 1 laws-article-paragraph-face)
	`(,laws-article-item-face-regexp 1 laws-article-item-face)
	`(,laws-article-subitem2-face-regexp 0 laws-article-subitem2-face)
	`(,laws-article-subitem3-face-regexp  0 laws-article-subitem3-face)
	`(,laws-article-subitem4-face-regexp 0 laws-article-subitem4-face)
	`(,laws-volume-face-regexp 1 laws-volume-face)
	`(,laws-comment-face-regexp 1 laws-comment-face)
	`(,laws-anchor-name-face-regexp2 ,(laws-set-mouse-face-2 2)))
  "Font lock keywords to highlight the `laws-mode' buffer.")

(unless laws-anchor-clickable
  (mapc (lambda (x)
	  (delete x laws-font-lock-keywords-1))
	(list `(,laws-anchor-name-face-regexp2 ,(laws-set-mouse-face-2 2))
	      `(,laws-anchor-article-face-regexp3 ,(laws-set-mouse-face-2 1))
	      `("同法" ,(laws-set-mouse-face-2 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; laws-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun laws-make-backup-file (file)
  (let ((version-control t))
    (when (file-exists-p file)
      (rename-file file (car (find-backup-file-name file))))))

(defun laws-make-index-files (&optional regenerate)
  (unless laws-online-mode
    (laws-online-mode-message #'error))
  (labels ((make-index (file new-alist-func old-alist-func)
	     ;; インデックスファイルを生成する関数。FILEに、NEW-ALIST-FUNCの
	     ;; 返すALISTを出力する。ALISTがOLD-ALIST-FUNCの返す値と同じなら
	     ;; 出力しない。出力する場合、番号付きバックアップファイルを生成
	     ;; する。戻り値は出力した場合はTを返し、出力しなければNILを返す。
	     (let ((alist (funcall new-alist-func)))
	       (if (equal alist (funcall old-alist-func))
		   nil
		 (laws-make-backup-file file)
		 (with-temp-file file
		   (insert (format "%S" alist))
		   (message "Wrote %s" file))
		 t))))
    (let (index-updatedp abbrev-updatedp)
      ;; indexファイルが存在しない場合
      ;; 再取得して更新された場合
      (when (or regenerate (not (file-exists-p laws-index-file)))
	(if (not (y-or-n-p "Make index files? "))
	    (if regenerate
		(error "Cancel.")
	      (error "First of all, you should make the index file."))
	  (laws-make-directory laws-path)
	  (setq index-updatedp
		(make-index laws-index-file #'laws-get-index #'laws-alist))
	  (message "Process has completed.")))
      ;; abbrevファイルが存在しない場合
      ;; 再取得して更新された場合
      (when (or regenerate (not (file-exists-p laws-abbrev-file)))
	(laws-make-directory laws-path)
	(setq abbrev-updatedp
	      (make-index
	       laws-abbrev-file #'laws-make-abbrev-index #'laws-abbrev))
	(message "Process has completed.")
	(sit-for 1))
      (cons index-updatedp abbrev-updatedp))))

(defun laws-url-retrieve (url)
  "URLをGETする。"
  (save-current-buffer
    (with-current-buffer
	(condition-case err
	    (url-retrieve-synchronously url)
	  (error
	   (error "Cannot retrieve URL: %s" url)))
      (let ((coding (detect-coding-region (point-min) (point-max) t)))
	(decode-coding-region (point-min) (point-max) coding)
	(set-buffer-multibyte t)
	(goto-char (point-min))
	(current-buffer)))))

(defun laws-get-dirname (id)
  "GETしたIDの保存先ディレクトリを返す。"
  (concat laws-htmldata-path "/" (upcase (substring id 0 3))))

(defvar laws-egov "http://law.e-gov.go.jp/cgi-bin/idxsearch.cgi"
  "法令データ提供システムのcgiのURL。")

(defun laws-get-index ()
  "事項別分類索引をGETして、法令名とIDのalistのリストを生成して返す。"
  (labels ((split (m)
	     (save-match-data
	       ;; htmldata
	       ;; 1. 法令名文字列の末尾が半角空白の場合がある。
	       ;; 2. （）が複数の場合があるが、0個の場合はない。
	       (let ((s (replace-regexp-in-string "[\r\n\t]+" "" m)))
		 (when (string-match "^\\(.+?\\)\\(　抄\\)?\\(（.+?）*\\) *$" s)
		   (cons (match-string 1 s) (concat (match-string 2 s)
						    (match-string 3 s))))))))
    (save-current-buffer
      (laws:map #'(lambda (request index)
		    (let ((case-fold-search t)
			  (result nil))
		      (with-current-buffer
			  (laws-url-retrieve (concat laws-egov "?" request))
			(message "Reading [text/html]... %2d of 50 (%d%%)"
				 (car index) (* (/ (car index) 50.0) 100))
			(while (re-search-forward
				"H_FILE_NAME=\\([^&]+\\)&[^>]+>\\([^<]+\\)</" nil t)
			  (push (cons (split (match-string 2)) (match-string 1))
				result))
			(kill-buffer (current-buffer))
			(cons (cdr index) (nreverse result)))))
		(laws-request-uri-list) laws-jikoubetsu-index-alist))))

(defvar laws-ryaku-url
  "http://law.e-gov.go.jp/cgi-bin/idxsearch.cgi?H_RYAKU_SUBMIT=ON")

(defun laws-make-abbrev-index ()
  "略称法令名をGETして、連想リストを返す。"
  (with-current-buffer (laws-url-retrieve laws-ryaku-url)
    (let ((rx-a "<A NAME=\"[0-9]+\"><B>\\(.+?\\)</B>")
	  (rx-b "<B>\\(.+?\\)</B>")
	  (rx-c "<A HREF=\".+?H_FILE_NAME=\\([^&]+\\)&.+?\">\\(.+?\\)</A>"))
      (let ((result-a nil))
	(while (re-search-forward rx-a nil t)
	  (push
	   (let ((lim (save-match-data
			(save-excursion
			  (or (and (re-search-forward rx-a nil t)
				   (line-beginning-position))
			      (point-max))))))
	     (cons (match-string 1)
		   (save-match-data
		     (let ((result-b nil))
		       (while (re-search-forward rx-b lim t)
			 (let ((lim (save-match-data
				      (save-excursion
					(or (and (re-search-forward rx-b lim t)
						 (line-beginning-position))
					    lim)))))
			   (push
			    (cons (match-string 1)
				  (save-match-data
				    (let ((result-c nil))
				      (while (re-search-forward rx-c lim t)
					(push (cons (match-string 2)
						    (match-string 1))
					      result-c))
				      (nreverse result-c))))
			    result-b)))
		       (nreverse result-b)))))
	   result-a))
	(nreverse result-a)))))

(defun laws-request-uri-list ()
  "URLリスト"
  (loop for (cid . name) in laws-jikoubetsu-index-alist
        collect 
        (format (mapconcat #'identity
                           '("H_CTG_%d=%%81%%40"
                             "H_CTG_GUN=1"
                             "H_NAME=0"
                             "H_NAME_YOMI=%%82%%A0"
                             "H_NO_GENGO=H"
                             "H_NO_YEAR=0"
                             "H_NO_TYPE=2"
                             "H_NO_NO=0"
                             "H_RYAKU=1"
                             "H_YOMI_GUN=1") "&")
                cid)))

(defconst laws-jikoubetsu-index-alist 
  '((1 . "憲法")	(2 . "国会")
    (3 . "行政組織")	(4 . "国家公務員")
    (5 . "行政手続")	(6 . "統計")
    (7 . "地方自治")	(8 . "地方財政")
    (9 . "司法")	(10 . "民事")
    (11 . "刑事")	(12 . "警察")
    (13 . "消防")	(14 . "国土開発")
    (15 . "土地")	(16 . "都市計画")
    (17 . "道路")	(18 . "河川")
    (19 . "災害対策")	(20 . "建築・住宅")
    (21 . "財務通則")	(22 . "国有財産")
    (23 . "国税")	(24 . "専売・事業")
    (25 . "国債")	(26 . "教育")
    (27 . "文化")	(28 . "産業通則")
    (29 . "農業")	(30 . "林業")
    (31 . "水産業")	(32 . "鉱業")
    (33 . "工業")	(34 . "商業")
    (35 . "金融・保険")	(36 . "外国為替・貿易")
    (37 . "陸運")	(38 . "海運")
    (39 . "航空")	(40 . "貨物運送")
    (41 . "観光")	(42 . "郵務")
    (43 . "電気通信")	(44 . "労働")
    (45 . "環境保全")	(46 . "厚生")
    (47 . "社会福祉")	(48 . "社会保険")
    (49 . "防衛")	(50 . "外事"))
  "事項別索引")

(defun laws-make-jikoubetsu-index ()
  (let ((strs nil)
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "NAME=\"H_CTG_\\([0-9]+\\)[^>]+>\\(.+?\\)<" nil t)
	(push (cons
	       (match-string-no-properties 1)
	       (replace-regexp-in-string
		"[ 　]+" "" (match-string-no-properties 2)))
	      strs)))
    (sort strs (lambda (x y)
		 (< (string-to-number (car x)) (string-to-number (car y)))))))

(defun laws-make-stags-alist ()
  "行頭から前方に検索して、ポイント行内のS式のリストを返す。"
  (save-excursion
    (save-match-data
      (let ((result nil)
	    (lim (line-end-position)))
	(forward-line 0)
	(while (re-search-forward "(:a [^()]+)" lim t)
	  (push (cons (match-beginning 0) (match-end 0)) result))
	result))))

(defun laws-in-stag-p (pos)
  "ポイントPOSがタグの中にあればtを、なければnilを返す。"
  (let ((ls (laws-make-stags-alist)))
    (block nil
      (while ls
	(let ((points (car ls)))
	  (if (and (> pos (car points))
		   (< pos (cdr points)))
	      (return t)
	    (pop ls)))))))

(defun laws-duplicate-names (ls)
  "重複する法令名のリストを返す。"
  (do ((xs (cdr ls) (cdr xs))
       (name (caar ls) (caar xs))
       (result nil))
      ((null xs) (delete-dups result))
    (do ((ys xs (cdr ys)))
	((null ys))
      (when (string-match name (caar ys))
	(push name result)))))

(defun laws-replace-table-value (&optional table-pixel)
  "GETしたhtmlのタグを置換する。"
  (let ((case-fold-search t)
	(pixel (or table-pixel laws-table-pixel)))
    (labels ((match (rx s) (save-match-data (string-match rx s))))
      (while (re-search-forward "<TABLE [^>]+>\\|<DIV ALIGN=\"right\">" nil t)
	(replace-match
	 (let ((s (match-string 0)))
	   (cond ((match "<DIV ALIGN=\"right\">" s)
		  ;; 冒頭の右寄せのテーブルを左寄せに。
		  "<DIV ALIGN=\"left\">")
		 ((match "TABLE WIDTH" s)
		  ;; 冒頭のテーブルの幅を指定。
		  (format "<TABLE WIDTH=%S BORDER=%S>" pixel 0))
		 (t ;; 出力される罫線表の幅を指定。
		  (format "<TABLE WIDTH=%S BORDER>" pixel)))))))))

(defun laws-load-index (index)
  "インデックスファイルをreadしてリストを返す。"
  (with-temp-buffer
    (insert-file-contents index)
    (do ((xs (read (current-buffer)) (cdr xs))
	 (acc nil))
	((null xs) acc)
      (do ((ys (cdar xs) (cdr ys)))
	  ((null ys))
	(push (cons (car (caar ys)) (cdar ys)) acc)))))

(defun laws-extract-name ()
  "html から法令名を取得して、文字列長の小さい順でソートされたリストを返す。"
  (let ((result nil)
	(case-fold-search t)
	(rx "<A HREF=.+?REF_NAME=\\([^&]+\\)&ANCHOR_F=&ANCHOR_T="))
    (while (re-search-forward rx nil t) (push (match-string 1) result))
    (sort (delete-dups result) (lambda (x y) (< (length x) (length y))))))

(defun laws-detect-coding-region (start end priority-list)
  ;; `w3m-detect-coding-region'(w3m-fsf.el)の関数名のみ変更。
  "Detect coding system of the text in the region between START and END.
Return the first possible coding system.

PRIORITY-LIST is a list of coding systems ordered by priority."
  (let (category categories)
    (dolist (codesys priority-list)
      (setq category (coding-system-category codesys))
      (unless (or (null category) (assq category categories))
	(push (cons category codesys) categories)))
    (with-coding-priority (nreverse categories)
      (car (detect-coding-region start end)))))


(defun laws-url-decode-string (str &optional coding)
  ;; `w3m-url-decode-string'(w3m.el)のxemacs対応を除いた他、関数名を変更。
  (let ((start 0)
	(buf))
    (while (string-match "+\\|%\\(0D%0A\\|\\([0-9a-fA-F][0-9a-fA-F]\\)\\)"
			 str start)
      (push (substring str start (match-beginning 0)) buf)
      (push (cond
	     ((match-beginning 2)
	      (vector (string-to-number (match-string 2 str) 16)))
	     ((match-beginning 1) "\n")
	     (t " "))
	    buf)
      (setq start (match-end 0)))
    (setq str (apply 'concat (nreverse (cons (substring str start) buf))))
    (setq str (string-make-unibyte str))
    (when (listp coding)
      (setq coding
	    (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (insert str)
	      (laws-detect-coding-region (point-min) (point-max) coding))))
    (decode-coding-string
     str (or coding 'iso-8859-1 'iso-2022-7bit 'iso-2022-7bit))))

(defun laws-make-substitution-alist (ls index)
  "置換用のalist \(\(\"法令名\" . \"path\"\) ...\) を返す。"
  (do ((ls ls (cdr ls))
       (acc nil (acons (car ls) (cdr (assoc (car ls) index)) acc)))
      ((null ls) acc)))

(defun laws-expand-htmldata-url (id)
  "ID(のファイル名部分`M29HO089'などの形式)から、法令名のURLを返す。"
  (if (> 3 (length id))
      ""
    (concat laws-egov-url laws-htmldata-directory "/"
	    (upcase (substring id 0 3)) "/" (upcase id) ".html")))

(defun laws-expand-htmldata-file (id)
  "ID(のファイル名部分)から、GETしたHTMLの保存先パスファイルを返す。"
  (if (> 3 (length id))
      ""
    (concat laws-htmldata-path "/"
	    (upcase (substring id 0 3)) "/" (upcase id) ".html")))

(defun laws-expand-data-file (id)
  "ID(のファイル名部分)から、ダンプしたデータの保存先パスファイル名を返す。"
  (if (> 3 (length id))
      ""
    (concat laws-data-path "/"
	    (downcase (substring id 0 3)) "/" (downcase id) laws-extention)))

(defun laws-expand-init-file (id)
  (if (> 3 (length id))
      ""
    (concat laws-data-path "/"
	    (downcase (substring id 0 3)) "/." (downcase id))))

(defun laws-make-directory (dir)
  (unless (and (file-exists-p dir)
	       (file-directory-p dir))
    (when (file-regular-p dir)
      (error "File `%s' exists!" dir))
    (make-directory dir 'parent)))

(defun laws-htmldata-retrieve (id force)
  "IDのHTMLデータが存在しない場合と、FORCEが非nilの場合に取得する。最後
に取得した時から変更があった場合、番号付きバックアップファイルを生成する。
保存先のhtmlのパスファイル名を返す。"
  (let ((html-path (laws-expand-htmldata-file id))
	(file (laws-expand-data-file id)))
    (when (or force (not (file-exists-p html-path)))
      (let ((buffer
	     (set-buffer (laws-url-retrieve (laws-expand-htmldata-url id)))))
	(with-current-buffer buffer
	  (goto-char (point-min))
	  ;; (save-excursion (replace-string "" ""))    ;Emacs23?
	  (when (search-forward "404 Not Found" nil t)
	    (re-search-forward "^$" nil t)
	    (error (concat (laws-expand-htmldata-url id)
			   (replace-regexp-in-string
			    "<.+?>\\|\r\n" ""
			    (buffer-substring (point) (point-max))))))
	  ;; ファイルが存在しない場合と、取得したデータと保存されているデータ
	  ;; を比較し、データが更新された場合、バックアップと出力を行う。
	  ;; 更新されていなければメッセージを出す。
	  (let (s1 e1 s2 e2)
	    (re-search-forward "^$" nil t)
	    (forward-line 1)
	    (setq s1 (point))
	    (setq e1 (point-max))
	    (if (= (with-temp-buffer
		     (if (not (file-exists-p html-path))
			 -1
		       (insert-file-contents html-path)
		       (setq s2 (point-min) e2 (point-max))
		       (compare-buffer-substrings buffer s1 e1 nil s2 e2)))
		   0)
		(progn
		  (message "File `%s.html' not changed from last retrieving." id)
		  (sit-for 2))
	      (when (file-exists-p file) (delete-file file)) ;テキストの削除
	      (laws-make-directory (laws-get-dirname id))    ;ディレクトリ
	      (laws-make-backup-file html-path)		     ;バックアップ
	      (write-region-as-coding-system 'japanese-shift-jis-dos
		(point) (point-max) html-path)))
	  (kill-buffer buffer))))
    html-path))

(defun laws-make-images-list ()
  "htmldataから画像データのパスリストを取得して返す。"
  (save-excursion
    (goto-char (point-min))
    (let ((result nil)
	  (case-fold-search t))
      (while (re-search-forward "<IMG SRC=\"\\(.+?\\)\" [^>]+>" nil t)
	(push (match-string 1) result))
      (nreverse result))))

(defun laws-expand-image-file-name (path)
  (concat laws-path path))

(defun laws-expand-image-file-url (path)
  (concat laws-egov-url (substring path 1)))

(defun laws-images-retrieve (ls)
  (save-current-buffer
    (mapcar #'(lambda (path)
		(let* ((url (laws-expand-image-file-url path))
		       (file (laws-expand-image-file-name path))
		       (dir (file-name-directory file))
		       (buffer (laws-url-retrieve url)))
		  (laws-make-directory dir)
		  (with-current-buffer (set-buffer buffer)
		    (goto-char (point-min))
		    (write-region-as-binary
		     (progn (re-search-forward "^$" nil t)
			    (forward-line 1)
			    (point))
		     (point-max)
		     file))
		  (kill-buffer buffer)))
	    ls)))

(defun laws-replace-image-tags (images)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while images
      (when (re-search-forward
	     (format "IMG SRC=\"%s\" ALT=\\(\"\"\\)" (car images)) nil t)
	(replace-match (format "\"<../..%s>\"" (car images)) nil nil nil 1))
      (pop images))))

(defun laws-html-get-h-path ()
  "htmldataからH-PATHを抽出する。"
  (let ((case-fold-search t)
	(h-path nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<A HREF=\".+?H_PATH=\\(.+?\\)\" [^>]+>" nil t)
	(let ((path (match-string-no-properties 1)))
	  (when (re-search-forward "\\([^<]+\\)</A>" nil t)
	    (let ((s (laws-url-decode-string (match-string 1))))
	      (push (cons s path) h-path)))))
      (nreverse h-path))))

(defun laws-w3m-dump (command htmldata)
  (shell-command (concat command " " htmldata) t))

(defun laws-replace-zspc ()
  (goto-char (point-min))
  (while (search-forward "  " nil t)
    (replace-match "　")))

(defun laws-make-font-lock-regexp-in-buffer (h-path)
  ;; laws-name-search-in-buffer
  (let ((xs (laws-names-list))
	(result nil))
    (save-excursion
      (while xs
	(goto-char (point-min))
	(when (search-forward (car xs) nil t)
	  (push (match-string 0) result))
	(pop xs)))
    (mapc (lambda (name)
	    (setq result (delete name result)))
	  laws-excluded-law-names)
    (when h-path
      (mapc (lambda (cell) (push (car cell) result))
	    h-path))
    (if (null result)
	nil
      (regexp-opt result))))

(defun laws-write-init-file (out h-path iimagep force)
  (if (or force (not (file-exists-p out)))
      (let ((regexps (laws-make-font-lock-regexp-in-buffer h-path)))
	(with-temp-file out
	  ;; 正規表現文字列、冒頭の未施行法令等とその参照先URLのcons、imageが
	  ;; 含まれているかどうか。
	  (insert ";;; `laws-mode' laws-font-lock-keywords-2 file.\n")
	  (insert (format "%S" (list regexps h-path iimagep)))
	  (message "Wrote %s" out)))))

(defun laws-read-init-file ()
  ;; 生成されたファイル情報を読み込む。
  (let ((file (laws-expand-init-file
	       (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name))))))
    (if (file-exists-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (read (current-buffer)))
      (message "Not exists laws-font-lock-keywords-2 file `%s'" file)
      nil)))

(defun laws-make-data (id &optional force)
  "htmlデータをw3mでダンプする。FORCEが非nilならIDをGET、nilなら既
にGETしたHTMLを対象とする。また、font-lockのためのタグの埋め込み等
の加工を行なう。
生成されたファイルの名前を返す。"
  (unless laws-online-mode
    (laws-online-mode-message #'error))
  (message "Getting file and converting...")
  (let ((temp (concat laws-temp-path "/temp.html"))
	;; htmldata を取得。
	(html (laws-htmldata-retrieve id nil))
	(file (laws-expand-data-file id))
	(regfile (laws-expand-init-file id))
	(coding-system-for-write (or laws-coding-system-for-write))
	alist images h-path)
    (with-temp-file temp
      (laws-make-directory (file-name-directory temp))
      ;; バッファにhtmlを取得する。
      (message "Getting htmldata...")
      (insert-file-contents-as-raw-text html)
      (message (concat (current-message) " done."))
      ;; イメージデータの取得。
      (setq images (laws-make-images-list))
      (when (and force images)
	(message "Getting images...")
	(laws-images-retrieve images)
	(message "Getting images... done."))
      ;; `H_PATH'の取得。
      (setq h-path (laws-html-get-h-path))
      ;; htmlタグの置換。テンポラリファイルに書き出す。
      ;; テンポラリファイルは削除せずに残す。
      (goto-char (point-min))
      (message "Replacing tag's value in htmldata...")
      (laws-replace-table-value)
      ;; イメージを置換する。
      (when images (laws-replace-image-tags images))
      )
    (message (concat (current-message) " done."))
    ;; テンポラリファイルを対象にw3mでダンプする。
    (with-temp-file file
      (laws-make-directory (file-name-directory file))
      (message "Extracting data from htmldata...")
      (laws-w3m-dump
       (funcall laws-w3m-dump-command laws-w3m-dump-cols) temp)
      (message (concat (current-message) " done."))
      ;; 半角空白2個を全角空白に置換する。
      (message "Replacing spaces...")
      (save-excursion (laws-replace-zspc))
      (message (concat (current-message) " done."))
      ;; バッファ内の法令名を取得し、正規表現を生成する。
      (message "Scanning law names...")
      ;; 情報を書き込む。
      (laws-write-init-file regfile h-path (if images t nil) force)
      (message (concat (current-message) " done."))
      (message "Scanning law names...done")
      (message "Getting file and converting...done")
      ;; 生成されたファイルの名前を返す。
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; laws-index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;
;; Header line
;;
(defun laws-index-mode-header-line-keymap (mode)
  "ヘッダラインのキーマップを返す。"
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
      `(lambda (e) (interactive "e")
	       (set-buffer (window-buffer (posn-window (event-end e))))
	       (laws-index-goto-mode (intern ,mode))))
    map))

(defun laws-index-header-line-format (mode)
  "`laws-index-mode'の。`header-line-format'"
  (labels ((spc (n)
	     (propertize " " 'display `(space :width (,n)))))
    (concat " "
	    (mapconcat
	     (lambda (s)
	       (if (string= s mode)
		   (concat 
		    (propertize (concat (spc 5) s (spc 5))
				'face
				laws-index-header-selected-face
				'mouse-face 'highlight
				'help-echo (format "`%s'" s))
		    (spc 1))
		 (concat
		  (propertize
		   (concat
		    (propertize (concat (spc 5) (substring s 0 1))
				'face
				laws-index-header-key-face)
		    (propertize (concat (substring s 1) (spc 5))
				'face
				laws-index-header-foreground-face))
		   'mouse-face 'highlight
		   'help-echo (format "mouse-1: Goto `%s' mode" s)
		   'local-map (laws-index-mode-header-line-keymap s))
		  (spc 1))))
	     laws-index-header-items ""))))

(defun laws-index-update ()
  "現在のモードの表示を更新する。
更新するのは、Opened,Recent,Bookmarkの場合。それ以外のモードでは更新しない。"
  (interactive)
  (when (memq laws-index-local-mode '(Opened Recent Bookmark))
    (laws-index-goto-mode laws-index-local-mode 'update)
    (message "Updating %s...done" laws-index-local-mode)))

(defun laws-index-goto-mode (mode &optional update)
  "`laws-index-mode'の各、個別のモード`laws-index-local-mode'に遷移する。
MODEが現在のMODEと同じ場合、nilを返す(see. `laws-index-search')。"
  (laws-index-set-mode-conf)
  (let ((name (lambda (mode)
		(format "%s:%s" laws-index-mode-name mode))))
    (unless (and (not update) (string= mode-name (funcall name mode)))
      (setq header-line-format
	    (if laws-use-index-header-line
		(laws-index-header-line-format mode)
	      nil))
      (setq mode-name (funcall name mode)
	    laws-index-local-mode mode)
      (force-mode-line-update)
      (laws-index-insert-contents mode)
      (laws-index-restore-mode-conf))))

;; Buffer configuration
(defun laws-index-set-mode-conf ()
  "現在のバッファの情報を保存する。"
  (setq laws-index-conf
	(delete (assoc laws-index-local-mode laws-index-conf)
		laws-index-conf))
  (push `(,laws-index-local-mode
	  ,(line-number-at-pos)
	  ,(window-start))
	laws-index-conf))

(defun laws-index-restore-mode-conf ()
  "以前のバッファの状態を復元する。"
  (let ((cel (assoc laws-index-local-mode laws-index-conf)))
    (when cel
      (destructuring-bind (mode line start)
	  (assoc laws-index-local-mode laws-index-conf)
	(set-window-start (selected-window) start)
	(laws-goto-line line)
	(laws-index-move-to-column)))))

;;
;; Common
;;

(defmacro laws-with-buffer-read-only (&rest forms)
  "バッファの未編集とリードオンリー状態を保持してFORMSを評価する。"
  '(unless (eq major-mode 'laws-index-mode)
    (error "ERROR: major-mode is not laws-index-mode."))
  `(progn
     (setq buffer-read-only nil)
     (unwind-protect
	  (save-excursion ,@forms)
       (progn
	 (setq buffer-read-only t)
	 (set-buffer-modified-p nil)))))

(defsubst laws-read ()
  "バッファのinvisibleなS式をreadする。"
  (save-excursion
    (forward-line 0)
    (read (current-buffer))))

(defsubst laws-get-values (&optional pointer)
  "バッファのinvisibleなS式のデータをリストで返す。
関数POINTERが与えられれば、POINTERが指す値を返す。
\(laws-read\)のエラーを把捉したらnilを返す。"
  (condition-case err
      (destructuring-bind (flag name . id) (laws-read)
	(if (functionp pointer)
	    (funcall pointer (list flag name id))
	  (list flag name id)))
    (error nil)))

(defsubst laws-any-function (funcs)
  "FUNCSの中で初めに非nilを返した関数を返す。
FUNCSは引数を取らない関数のリスト。"
  (block nil
    (mapc (lambda (f) (and (funcall f) (return f)))
	  funcs)))

(defun laws-goto-line (line)
  (goto-char (point-min))
  (forward-line line))

(defun laws:filter (pred ls)
  "PREDを適用した結果tを返した要素を集める。"
  (do ((xs ls (cdr xs))
       (acc nil (if (funcall pred (car xs)) (cons (car xs) acc) acc)))
      ((null xs) (nreverse acc))))

(defun laws:append-map (f ls &rest more)
  (apply 'append (apply 'laws:map f ls more)))

(defun laws:fringe (tree)
  (if (listp tree)
      (laws:append-map 'laws:fringe tree)
    (list tree)))

(defun laws:map (f ls &rest more)
  (let* ((x (cons ls more))
	 (n (apply 'min (mapcar 'length x))))
    (do ((n n (- n 1))
	 (x x (mapcar 'cdr x))
	 (ans nil (cons (apply f (mapcar 'car x)) ans)))
	((= n 0) (nreverse ans)))))

(unless (fboundp 'cl-set-nthcdr)
  (defun cl-set-nthcdr (n list x)
    (if (<= n 0) x (setcdr (nthcdr (1- n) list) x) list)))

;;
;; Load data
;;

;; Common

;; インデックスファイルの内容を保持するローカル変数。
(defun laws-alist ()
  "インデックスファイルをロードする関数。"
  (or laws-alist
      (and (file-exists-p laws-index-file)
	   (setq laws-alist
		 (with-temp-buffer
		   (insert-file-contents laws-index-file)
		   (read (current-buffer)))))))

;; 略称法令名のインデックスファイルの内容を保持するローカル変数。
(defun laws-abbrev ()
  "略称法令名のインデックスファイルをロードする関数。"
  (or laws-abbrev
      (and (file-exists-p laws-abbrev-file)
	   (setq laws-abbrev
		 (with-temp-buffer
		   (insert-file-contents laws-abbrev-file)
		   (read (current-buffer)))))))

;; Search
(defun laws-names-alist ()
  (or laws-names-alist
      (setq laws-names-alist
	    (cons (do ((xs (laws-alist) (cdr xs))
		       (result nil))
		      ((null xs) result)
		    (do ((ys (cdar xs) (cdr ys)))
			((null ys))
		      (push (car ys) result)))
		  (do ((xs (laws-abbrev-alist) (cdr xs))
		       (result nil))
		      ((null xs) result)
		    (do ((ys (cdr (cdar xs)) (cdr ys)))
			((null ys))
		      (push (cons (caar ys) (cdr (cdar ys))) result)))))))

(defun laws-search-alist ()
  laws-search-alist)

;; Index
;; `laws-alist'から生成した、`laws-index'のIndexモードで利用する連想リスト。
(defun laws-index-alist ()
  "`laws-index-alist'を生成する関数。"
  (or laws-index-alist
      (setq laws-index-alist
	    (do ((xs (laws-alist) (cdr xs))
		 (result nil))
		((null xs) (nreverse result))
	      (push (cons (caar xs)
			  (cons nil
				(do ((ys (cdar xs) (cdr ys))
				     (acc nil))
				    ((null ys) (nreverse acc))
				  (push (cons (concat (car (caar ys)) (cdr (caar ys)))
					      (cdar ys))
					acc))))
		    result)))))

;; Directory
;; `laws-alist'から生成した、`laws-index'のDirectoryモードで利用する連想リスト。
(defun laws-directory-alist ()
  "`laws-directory-alist'を生成する関数。"
  (or laws-directory-alist
      (setq laws-directory-alist
	    (let ((dirs (do ((xs (laws-alist) (cdr xs))
			     (result nil))
			    ((null xs)
			     (sort result
				   (lambda (x y) (string< (car x) (car y)))))
			  (let ((category (caar xs)))
			    (do ((ys (cdar xs) (cdr ys)))
				((null ys))
			      (let ((gengo (substring (cdar ys) 0 3)))
				(push (cons gengo
					    (cons (concat category ":"
							  (car (caar ys))
							  (cdr (caar ys)))
						  (cdar ys)))
				      result)))))))
	      (let* ((s0 (make-string 2 0))
		     ;; dirsをソートする比較関数
		     (compfun (lambda (x y)
				(let ((s1 (aref (car x) 0))
				      (s2 (aref (car y) 0)))
				  (if (= s1 s2)
				      (let ((n1 (string-to-number
						 (progn
						   (aset s0 0 (aref (car x) 1))
						   (aset s0 1 (aref (car x) 2))
						   s0)))
					    (n2 (string-to-number
						 (progn
						   (aset s0 0 (aref (car y) 1))
						   (aset s0 1 (aref (car y) 2))
						   s0))))
					(> n1 n2))
				    (cond ((or (= s1 ?H)
					       (= s2 ?H)) nil)
					  ((or (= s1 ?M)
					       (= s2 ?M)) t)
					  ((or (= s1 ?S)
					       (= s2 ?S)) nil)
					  (t t)))))))
		;; 同じ年数の要素を集める。
		(let ((result nil)
		      (ls (sort dirs compfun)))
		  (while ls
		    (let ((tmp (caar ls)))
		      (push (cons tmp
				  (cons nil ; closed flag
					(let ((acc nil))
					  (while (string= (caar ls) tmp)
					    (push (cdar ls) acc)
					    (pop ls))
					  acc)))
			    result)))
		  (nreverse result)))))))

;; Abbrev
;; `laws-abbrev'から生成した、`laws-index'のAbbrevモードで利用する連想リスト。
(defun laws-abbrev-alist ()
  "`laws-abbrev-alist'を生成する関数。"
  (or laws-abbrev-alist
      (setq
       laws-abbrev-alist
       (do ((xs (laws-abbrev) (cdr xs))
	    (result nil))
	   ((null xs) (nreverse result))
	 (push (cons (caar xs)
		     (cons nil		; closed flag
			   (do ((ys (cdar xs) (cdr ys))
				(acc nil))
			       ((null ys) (nreverse acc))
			     (push (cons (caar ys)
					 (cons nil ; closed flag
					       (do ((zs (cdar ys) (cdr zs))
						    (acc nil))
						   ((null zs) (nreverse acc))
						 (push (cons (caar zs)(cdar zs))
						       acc))))
				   acc))))
	       result)))))

;; Bookmark
(defun laws-bookmark-alist ()
  "ブックマークの連想リストを返す関数。"
  (or laws-bookmark-alist
      (and (file-exists-p laws-bookmark-file)
	   (setq laws-bookmark-alist
		 (with-temp-buffer
		   (insert-file-contents laws-bookmark-file)
		   (read (current-buffer)))))))

(defun laws-bookmark-save ()
  "ブックマークファイル:`laws-bookmark-file'に
`laws-bookmark-alist'を出力する。変更がなかった場合は出力しない。"
  (ignore-errors
    (when (and laws-bookmark-file
	       (file-exists-p (file-name-directory laws-bookmark-file)))
      (with-temp-buffer
	(save-excursion
	  (if (file-exists-p laws-bookmark-file)
	      (insert-file-contents laws-bookmark-file)
	    (princ nil (current-buffer))))
	(unless (equal (read (current-buffer)) (laws-bookmark-alist))
	  (with-temp-file laws-bookmark-file
	    (insert (format "%S" laws-bookmark-alist))
	    (message "Wrote %s" laws-bookmark-file))
	  t)))))

(defun laws-convert-files ()
  "Bookmark's format change in v0.8.5 from v0.8.4."
  ;; Bookmark
  (when (and laws-bookmark-file
	     (file-exists-p (file-name-directory laws-bookmark-file))
	     (file-exists-p laws-bookmark-file))
    (with-temp-buffer
      (save-excursion (insert-file-contents laws-bookmark-file))
      (let ((alist (read (current-buffer))))
	(when (consp (car alist))
	  (with-temp-file laws-bookmark-file
	    (insert (format "%S" (mapcar (lambda (x) (upcase (cdr x))) alist)))
	    (message "Wrote %s" laws-bookmark-file))
	  (setq laws-bookmark-alist nil)
	  (laws-bookmark-alist)))))
  ;; Recent
  (when (and laws-recent-file
	     (file-exists-p (file-name-directory laws-recent-file))
	     (file-exists-p laws-recent-file))
    (with-temp-buffer
      (save-excursion (insert-file-contents laws-recent-file))
      (let ((alist (read (current-buffer))))
	(with-temp-file laws-recent-file
	  (insert (format "%S" (mapcar (lambda (x) (upcase x)) alist))))
	(message "Wrote %s" laws-recent-file)
	(setq laws-recent-alist nil)
	(laws-recent-alist)))))

;; Opened

(defun laws-opened-alist ()
  "現在開いている法令データの連想リストを返す。"
  ;; 法令データかどうかは、パスファイル名が法令データのパスファイル名と等しい
  ;; かどうかで判定。ディレクトリは大文字、urlは小文字。
  (laws-make-alist-from-name
   (lambda ()
     (labels ((nameof (file) (laws-file-sans-name file)))
       (mapcar #'nameof
	       (laws:filter
		(lambda (file)
		  (string= (laws-expand-data-file (nameof file))
			   file))
		(delete nil (mapcar #'buffer-file-name (buffer-list)))))))))

;; Recent
(defun laws-recent-alist ()
  "最近開いたファイルの連想リストを返す。"
  (or laws-recent-alist
      (and (file-exists-p laws-recent-file)
	   (setq laws-recent-alist
		 (with-temp-buffer
		   (insert-file-contents laws-recent-file)
		   (read (current-buffer)))))))

(defun laws-recent-add ()
  (ignore-errors
    (let ((name (upcase (laws-file-sans-name (buffer-file-name)))))
      (when (string= (buffer-file-name)
		     (laws-expand-data-file name))
	(setq laws-recent-alist (cons name (delete name (laws-recent-alist))))
	(when (> (length laws-recent-alist) laws-recent-max)
	  (setq laws-recent-alist
		(butlast laws-recent-alist
			 (- (length laws-recent-alist) laws-recent-max))))))))

(defun laws-recent-save ()
  "最近開いた法令ファイル: `laws-recent-file'に
`laws-recent-alist'を出力する。変更がなかった場合は出力しない。"
  (ignore-errors
    (when (and laws-recent-file
	       (file-exists-p (file-name-directory laws-recent-file)))
      (with-temp-buffer
	(save-excursion
	  (if (file-exists-p laws-recent-file)
	      (insert-file-contents laws-recent-file)
	    (princ nil (current-buffer))))
	(unless (equal (read (current-buffer)) (laws-recent-alist))
	  (with-temp-file laws-recent-file
	    (insert (format "%S" laws-recent-alist))
	    (message "Wrote %s" laws-recent-file))
	  t)))))

;;
;; Insert contents
;;
(defun laws-index-insert-contents (mode)
  "各モードごとにツリーの挿入処理を分岐する。"
  ;;(laws-save-)
  (laws-with-buffer-read-only (erase-buffer))
  (case mode
    (Opened	(laws-index-insert-opened))
    (Recent	(laws-index-insert-recent))
    (Search	(laws-index-insert-search))
    (Bookmark	(laws-index-insert-bookmark))
    (Index	(laws-index-insert-index))
    (Directory	(laws-index-insert-directory))
    (Abbrev	(laws-index-insert-abbrev))))

;; Common
(defun laws-make-alist-from-name (lfunc)
  "NAME(\"M29HO089\"のような形式)から法令名とNAMEの連想リストを生成する関数。
LFUNCは、NAMEからなるリストを返す関数。"
  (mapcar (lambda (name)
	    (or (block nil
		  (do ((xs (laws-alist) (cdr xs)))
		      ((null xs))
		    (let ((cell (rassoc (upcase name) (cdar xs))))
		      (when cell
			(return (cons (concat (caar cell) (cdar cell))
				      (cdr cell)))))))
		(cons "未登録法令" name)))
	  (funcall lfunc)))

(defun laws-index-insert-alist-function (func)
  "Index,Directoryで、ツリーの挿入処理をする関数。"
  (let ((alist (funcall func)))
    (case laws-index-local-mode
      ((Index Directory)
       (laws-with-buffer-read-only
	;; Test:
	;; (error Lisp nesting exceeds `max-lisp-eval-depth')
	;; (laws-index-search-insert-func alist)
	(while alist
	  (let ((cel (car alist))
		(opened (car (cdar alist))))
	    (insert (format "%S\n" `(,(if opened "-" "+") ,(car cel))))
	    (when opened
	      (do ((xs (cddr cel) (cdr xs)))
		  ((null xs))
		(insert (format "%S\n" `("  -" ,(caar xs) ,(cdar xs)))))))
	  (pop alist)))
       )
      ((Abbrev)
       (laws-with-buffer-read-only
	;; Test:
	;; (error Lisp nesting exceeds `max-lisp-eval-depth')
	;; (laws-index-search-insert-func alist)
	(do ((xs alist (cdr xs)))
	    ((null xs))
	  (let ((opened (car (cdar xs))))
	    (insert (format "%S\n" `(,(if opened "-" "+") ,(caar xs))))
	    (when opened
	      (do ((ys (cdr (cdar xs)) (cdr ys)))
		  ((null ys))
		(let ((opened (car (cdar ys)))) ;error
		  (insert (format "%S\n" `(,(if opened "  -" "  +") ,(caar ys))))
		  (when opened
		    (do ((zs (cdr (cdar ys)) (cdr zs)))
			((null zs))
		      (insert (format "%S\n" `("    -" ,(caar zs) ,(cdar zs))))))))))))
       )
      ((Bookmark Opened Recent)
       (laws-with-buffer-read-only
	(while alist
	  (insert (format "%S\n" `(" -" ,(caar alist) ,(cdar alist))))
	  (pop alist))))
      ((Search)
       (laws-with-buffer-read-only
	(laws-index-search-insert-func alist))
       (laws-index-highlight-search-buffer))
      )))

;; recursion
(defun laws-index-search-insert-func (alist)
  (unless (null alist)
    (let* ((cell (car alist))
	   (opened (cadr cell)))
      ;; 検索式
      (insert (format "%S\n" `(,(if opened "-" "+") ,(car cell))))
      ;; 完全一致,略称法令名検索,法令名検索結果を再帰的に挿入
      (labels
	  ((rec (ls)
	     (unless (null ls)
	       (let* ((cell (car ls))
		      (opened (cadr cell)))
		 (insert
		  (format "%S\n" `(,(if opened "  -" "  +") ,(car cell))))
		 (when opened
		   (let ((cell (cddr cell)))
		     (do ((xs cell (cdr xs)))
			 ((null xs))
		       (if (atom (cdar xs))
			   (insert
			    (format "%S\n" `("    -" ,(caar xs) ,(cdar xs))))
			 (let ((opened (car (cdar xs))))
			   (insert (format "%S\n" `(,(if opened "    -" "    +")
						     ,(caar xs))))
			   (when opened
			     (do ((ys (cdr (cdar xs)) (cdr ys)))
				 ((null ys))
			       (insert
				(format "%S\n" `("      -" ,(caar ys)
							   ,(cdar ys))))))))))))
	       (rec (cdr ls)))))
	(when opened (rec (cddr cell)))))
    (laws-index-search-insert-func (cdr alist))))

;; Opened
(defun laws-index-insert-opened ()
  (laws-index-insert-alist-function #'laws-opened-alist))

;; Recent
(defun laws-index-insert-recent ()
  (laws-index-insert-alist-function
   (lambda () (laws-make-alist-from-name #'laws-recent-alist))))

;; Search
(defun laws-index-insert-search ()
  (laws-index-insert-alist-function #'laws-search-alist))

;; Bookmark
(defun laws-index-insert-bookmark ()
  (laws-index-insert-alist-function
   (lambda () (laws-make-alist-from-name #'laws-bookmark-alist))))

;; Index
(defun laws-index-insert-index ()
  "Indexで、バッファにツリーを挿入する。"
  (laws-index-insert-alist-function #'laws-index-alist))

;; Directory
(defun laws-index-insert-directory ()
  "Directoryで、バッファにツリーを挿入する。"
  (laws-index-insert-alist-function #'laws-directory-alist))

;; Abbrev
(defun laws-index-insert-abbrev ()
  "Abbrevで、バッファにツリーを挿入する。"
  (laws-index-insert-alist-function #'laws-abbrev-alist))

;;
;; Open or close
;;

;; Common
(defsubst laws-index-folder-level ()
  (let ((level (member (laws-get-values #'car)
		       '("+" "-" "  +" "  -" "    +" "    -" "      -"))))
    (if level
	(/ (1- (length (car level))) 2)
      -1)))

(defsubst laws-index-folder-level-0 ()
  "folderの階層が最上位なら非nilを返す。"
  (zerop (laws-index-folder-level)))

(defsubst laws-index-folder-level-1 ()
  "folderの階層が1番目なら非nilを返す。"
  (= (laws-index-folder-level) 1))

(defsubst laws-index-folder-level-2 ()
  "フォルダの階層が2番目なら非nilを返す。"
  (= (laws-index-folder-level) 2))

(defsubst laws-index-folder-level-3 ()
  "フォルダの階層が3番目なら非nilを返す。"
  (= (laws-index-folder-level) 3))

(defsubst laws-index-not-folder-p ()
  "アイテムがフォルダでないとき真を返す。"
  (and (not (laws-index-folder-level-0))
       (not (laws-index-folder-level-1))
       (not (laws-index-folder-level-2))))

(defsubst laws-index-folder-open-p ()
  "folderが開いていれば非nilを、閉じていればnilを返す。"
  (save-excursion
    (forward-line 0)
    (and (re-search-forward "\" *-\"" (line-end-position) t) t)))

(defun laws-index-open-or-close ()
  "フォルダなら開閉し、法令ならその法令を開くコマンド。"
  (interactive)
  (apply #'funcall
	 (let ((mode laws-index-local-mode))
	   (case mode
	     (Opened		`(laws-index-opened-oc))
	     (Recent		`(laws-index-recent-oc))
	     (Search		`(laws-index-search-oc))
	     (Bookmark		`(laws-index-bookmark-oc))
	     (Index		`(laws-index-index-oc))
	     (Directory		`(laws-index-directory-oc))
	     (Abbrev		`(laws-index-abbrev-oc)))))
  ;; 開いた場合、次が実行されるのは問題か。
  (laws-index-move-to-column))

(defun laws-index-browse-at-point ()
  ;; メニューコマンド
  (interactive)
  (let ((id (car (nth 2 (laws-get-values)))))
    (if id
	(browse-url (laws-expand-htmldata-url id))
      (message "No url at point."))))

(defun laws-index-mouse-open-or-close (event)
  "マウスでフォルダの開閉、法令を開くコマンド。"
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (laws-index-open-or-close)
  (when (laws-index-folder-level-0) (recenter 0)))

(defun laws-retrieve-index ()
  "インデックスファイルを取得し直す。最後に取得してから更新があっ
た場合、番号付きバックアップを生成する。"
  (interactive)
  (unless (eq major-mode 'laws-index-mode)
    (error "Try in laws-index-mode."))
  (let ((updated (laws-make-index-files 'regenerate))
	msg)
    ;; Abbreves
    (cond ((cdr updated)
	   (setq laws-abbrev-alist nil)
	   (push "Abbrevs was updated." msg))
	  (t (push "Abbrevs was not updated." msg)))
    ;; Index
    (cond ((car updated)
	   (setq laws-alist nil)
	   (push "Index was updated." msg))
	  (t (push "Index was not updated." msg)))
    (when (and (or (car updated) (cdr updated))
	       (get-buffer laws-index-buffer))
      (kill-buffer laws-index-buffer)
      (laws-index))
    (message (mapconcat 'identity msg "  "))))

(defun laws-retrieve-html ()
  "ポイント位置のHTMLデータを再取得。最後に取得してから更新があっ
た場合、番号付きバックアップを生成する。"
  (interactive)
  (let ((id (cond ((eq major-mode 'laws-index-mode)
		   (car (nth 2 (laws-get-values))))
		  ((eq major-mode 'laws-mode)
		   (upcase (laws-file-sans-name (buffer-file-name))))
		  (t (error "Try in laws-index-mode or laws-mode.")))))
    (when (and (laws-get-name id)
	       (or laws-online-mode
		   (laws-online-mode-message #'error))
	       (y-or-n-p "Retrieve htmldata from egov? "))
      (let ((html (laws-expand-htmldata-file id))
	    (file (laws-expand-data-file id)))
	(laws-htmldata-retrieve id 'force)
	(let ((buffer (get-file-buffer file)))
	  (when (and buffer
		     ;; 更新された場合 => nil
		     (not (verify-visited-file-modtime buffer)))
	    (kill-buffer buffer)))
	(laws-open-file id)))))

(defun laws-index-open-all ()
  "`Index'で、すべてのフォルダを開くコマンド。"
  (interactive)
  (apply #'funcall
	 (let ((mode laws-index-local-mode))
	   (case mode
	     ;;(Opened `(laws-index-opened-oc ,mode))
	     ;;(Recent `(laws-index-recent-oc ,mode))
	     ;;(Search `(laws-index-search-oc ,mode))
	     ;;(Bookmark `(laws-index-bookmark-oc ,mode))
	     (Index `(laws-index-index-oc-all t))
	     (Directory `(laws-index-directory-oc-all t))
	     (Abbrev `(laws-index-abbrev-oc-all t))
	     (t (error "Not supported."))))))

(defun laws-index-close-all ()
  "`Index'で、すべてのフォルダを閉じるコマンド。"
  (interactive)
  (apply #'funcall
	 (let ((mode laws-index-local-mode))
	   (case mode
	     ;;(Opened `(laws-index-opened-oc nil))
	     ;;(Recent `(laws-index-recent-oc nil))
	     ;;(Search `(laws-index-search-oc nil))
	     ;;(Bookmark `(laws-index-bookmark-oc nil))
	     (Index `(laws-index-index-oc-all nil))
	     (Directory `(laws-index-directory-oc-all nil))
	     (Abbrev `(laws-index-abbrev-oc-all nil))
	     (t (error "Not supported."))))))

(defun laws-index-folder-toggle-state ()
  "フォルダの開閉フラグをトグルする。"
  (forward-line 0)
  (when (re-search-forward "[+-]" (line-end-position) t)
    (replace-match
     (string (+ (- ?- (string-to-char (match-string 0))) ?+)))))

(defun laws-index-upper-level ()
  "ひとつ上の階層に移動するコマンド。"
  (interactive)
  (let ((level (laws-index-folder-level)))
    (when (> level 0)
      (while (> (1+ (laws-index-folder-level)) level)
	(forward-line -1))
      (laws-index-move-to-column))))

;; Index and Directory
(defun laws-index-set-alist (name opened func)
  "連想リストのキーがNAMEのフォルダの開閉フラグを(not opened)にセットする。
FUNCは連想リストを返す関数。"
  (let ((cell (cdr (assoc name (funcall func)))))
    (setcar cell (not opened))
    (cdr cell)))

(defun laws-index-folder (name opened func)
  "`Index'と`Directory'で、フォルダの開閉を処理する関数。"
  (let ((cell (laws-index-set-alist name opened func)))
    (if opened
	(laws-with-buffer-read-only
	 (let ((start (progn (forward-line 1) (point)))
	       (end (progn (while (and (not (eobp))
				       (not (laws-index-folder-level-0)))
			     (forward-line 1))
			   (point))))
	   (unless (= start end)
	     (delete-region start end)
	     (forward-line -1)
	     (laws-index-folder-toggle-state))))
      ;; closed
      (laws-with-buffer-read-only
       (laws-index-folder-toggle-state)
       (forward-line 1)
       (do ((xs cell (cdr xs)))
	   ((null xs))
	 (insert (format "%S\n" `("  -" ,(caar xs) ,(cdar xs)))))
       (laws-index-upper-level)))))

(defun laws-open-file (id)
  (let* ((file (laws-expand-data-file id))
	 (buffer (get-file-buffer file)))
    (switch-to-buffer
     (or buffer
	 (prog1 (set-buffer
		 (find-file-noselect
		  (if (file-exists-p file)
		      file
		    (laws-make-data id 'force))))
	   (laws-mode))))
    (laws-recent-add)))

(defun laws-index-index-oc-function (func)
  "`Index',`Directory'で、フォルダなら開閉をし法令名なら開く。"
  (let* ((values (laws-get-values))
	 (name (cadr values))
	 (id (car (nth 2 values))))
    (if (laws-index-folder-level-0)
	;; folder open or close
	(laws-index-folder name (laws-index-folder-open-p) func)
      ;; file open
      (unless id (error "Not a law data."))
      (save-excursion
	(forward-line 0)
	(laws-open-file id)))))

(defun laws-index-index-oc-all-function (open afunc ifunc)
  "`Index',`Directory'で、すべてのフォルダの開閉をする。
AFUNCは連想リストを返す関数。IFUNCはツリーの挿入処理をする関数。"
  (save-excursion
    (do ((alist (funcall afunc) (cdr alist)))
	((null alist))
      (setcar (cdar alist) open))
    (laws-with-buffer-read-only (erase-buffer))
    (funcall ifunc)))

;; Opened

;; Recent

(defun laws-index-search-oc ()
  "`Search'で、フォルダなら開閉をし、法令名なら開く。"
  (let ((name (laws-get-values #'cadr))
	(keys nil))
    (unless name (error "Not a law data."))
    (if (laws-get-values (lambda (x) (nth 2 x)))
	(laws-open-file (car (laws-get-values (lambda (x) (nth 2 x)))))
      (let ((cell (save-excursion
		    (dotimes (x (laws-index-folder-level))
		      (laws-index-upper-level)
		      (push (laws-get-values #'cadr) keys))
		    (assoc (laws-get-values #'cadr) (laws-search-alist)))))
	(laws-index-set-search-alist
	 cell keys name (laws-index-folder-open-p))))))

(defun laws-index-set-search-alist (cell keys name opened)
  "`laws-search-alist'のCELLの中でキーがNAMEのフォルダの開閉フラグ
を(not opened)に変更してバッファを更新する。"
  (case (length keys)
    (0 (setcar (cdr cell) (not opened)))
    (1 (setcar (cdr (assoc name cell)) (not opened)))
    (2 (setcar (cdr (assoc name (cdr (assoc (cadr keys) cell))))
	       (not opened))))
  (unless (laws-index-goto-mode 'Search)
    (let ((line (line-number-at-pos)))
      (laws-with-buffer-read-only
       (erase-buffer)
       (laws-index-search-insert-func (laws-search-alist)))
      (laws-index-highlight-search-buffer)
      (laws-goto-line line))))

;; Opened
(defun laws-index-opened-oc ()
  "`Opened'で、法令ファイルに切り替える。"
  (laws-index-index-oc-function #'laws-opened-alist))

;; Recent
(defun laws-index-recent-oc ()
  "`Recent'で、法令ファイルに切り替える。"
  (laws-index-index-oc-function
   (lambda () (laws-make-alist-from-name #'laws-recent-alist))))

;; Bookmark
(defun laws-index-bookmark-oc ()
  "`Bookmark'で、法令ファイルを開く。"
  (laws-index-index-oc-function
   (lambda () (laws-make-alist-from-name #'laws-bookmark-alist))))

;; Index
(defun laws-index-index-oc ()
  "`Index'で、フォルダの開閉をする。"
  (laws-index-index-oc-function #'laws-index-alist))

(defun laws-index-index-oc-all (open)
  "`Index'で、すべてのフォルダの開閉をする。"
  (laws-index-index-oc-all-function
   open #'laws-index-alist #'laws-index-insert-index))

;; Directory
(defun laws-index-directory-oc ()
  "`Directory'で、フォルダの開閉をする。"
  (laws-index-index-oc-function #'laws-directory-alist))

(defun laws-index-directory-oc-all (open)
  "`Directory'で、すべてのフォルダの開閉をする。"
  (laws-index-index-oc-all-function
   open #'laws-directory-alist #'laws-index-insert-directory))

;; Abbrev
(defun laws-index-set-abbrev-alist (name opened)
  "`laws-abbrev-alist'のフォルダの開閉フラグをセットする関数。"
  (cond ((laws-index-folder-level-1)
	 ;; sub folder
	 (save-excursion
	   (laws-index-upper-level)
	   (let ((cell
		  (cdr (assoc (laws-get-values #'cadr) (laws-abbrev-alist)))))
	     (let ((cell (cdr (assoc name cell))))
	       (setcar cell (not opened))
	       (cdr cell)))))
	((laws-index-folder-level-0)
	 ;; folder
	 (let ((cell (cdr (assoc name (laws-abbrev-alist)))))
	   (setcar cell (not opened))
	   (cdr cell)))))

(defun laws-index-abbrev-folder (name opened sub)
  "`Abbrev'で、フォルダの開閉を処理する関数。"
  (let ((cell (laws-index-set-abbrev-alist name opened)))
    (unless (> (laws-index-folder-level) 1)
      (if opened
	  (laws-with-buffer-read-only
	   (let ((start (progn (forward-line 1) (point)))
		 (end (progn (while (and (not (eobp))
					 (not (if sub
						  (/= (laws-index-folder-level) 2)
						(laws-index-folder-level-0))))
			       (forward-line 1))
			     (point))))
	     (unless (= start end)
	       (delete-region start end)
	       (forward-line -1)
	       (laws-index-folder-toggle-state))))
	;; closed
	(laws-with-buffer-read-only
	 (laws-index-folder-toggle-state)
	 (forward-line 1)
	 (if sub
	     ;; sub folder
	     (do ((zs cell (cdr zs)))
		 ((null zs))
	       (insert (format "%S\n" `("    -" ,(caar zs) ,(cdar zs)))))
	   ;; folder
	   (do ((ys cell (cdr ys)))
	       ((null ys))
	     (let ((opened (car (cdar ys))))
	       (insert (format "%S\n" `(,(if opened "  -" "  +") ,(caar ys))))
	       (when opened
		 (do ((zs (cdr (cdar ys)) (cdr zs)))
		     ((null zs))
		   (insert (format "%S\n" `("    -" ,(caar zs) ,(cdar zs)))))))))
	 (laws-index-upper-level))))))

(defun laws-index-abbrev-oc ()
  "`Abbrev'で、フォルダなら開閉し法令なら開く。"
  (let* ((values (laws-get-values))
	 (name (cadr values))
	 (id (nth 2 values)))
    (if (< (laws-index-folder-level) 2)
	;; menu open or close
	(laws-index-abbrev-folder
	 name (laws-index-folder-open-p) (laws-index-folder-level-1))
      ;; file open
      (laws-open-file (car id)))))

(defun laws-index-abbrev-oc-all (open)
  "`Abbrev'で、すべてのフォルダの開閉をする。"
  (laws-index-index-oc-all-function
   open #'laws-abbrev-alist #'laws-index-insert-abbrev))

;;
;; Search
;;
(defalias 'laws-search 'laws-index-search
  "法令名(略称法令名を含む)を検索するコマンド。引数付きで実行した
場合は、以前の検索結果を初期化しない。")

(defun laws-index-search (rx &optional noclear)
  "法令名(略称法令名を含む)を検索するコマンド。引数付きで実行した
場合は、以前の検索結果を初期化しない。"
  (interactive (laws-index-search-interactive))
  (labels
      ((display-result (rx names abbreves complete noclear)
	 (unless noclear (setq laws-search-alist nil))
	 ;; t: opened flag
	 (push
	  (list (format "検索式 `%s'" rx) t
		`(,(format "法令名完全一致 該当件数 %d" (length complete)) t ,@complete)
		`(,(format "略称法令名検索 該当件数 %d" (length abbreves)) t ,@abbreves)
		`(,(format "法令名検索 該当件数 %d" (length names)) t ,@names))
	  laws-search-alist)
	 ;; バッファ更新
	 ;; laws-index-goto-mode: Return nil if same local-mode.
	 (unless (laws-index-goto-mode 'Search)
	   (laws-with-buffer-read-only (erase-buffer))
	   (laws-index-insert-alist-function #'laws-search-alist))))
    (let ((complete nil))
      (message "Searching...")
      (display-result
       ;; 検索式
       rx
       ;; 法令名検索
       (do ((xs (car (laws-names-alist)) (cdr xs))
	    (names-search nil))
	   ((null xs) names-search)
	 (let ((name (car (caar xs)))
	       (name2 (cdr (caar xs))))
	   ;; 民法（民法第一編第二編第三編）（明治二十九年四月二十七日法律第八十九号）
	   ;; のうち、括弧を除いた部分の検索。
	   (when (string-match rx name)
	     (let ((match (cons (concat name (cdr (caar xs))) (cdar xs))))
	       (if (string= name rx)
		   ;; 完全一致(民法など複数マッチする場合がある)
		   (push match complete)
		 ;; 一部一致
		 ;; 完全一致、また既に一部一致に含まれる場合は、consしない。
		 (unless (or (member match names-search)
			     (member match complete))
		   (push match names-search)))))
	   ;; 後半の括弧部分の検索(括弧内も検索対象に入れる)。
	   ;; 完全一致、また既に一部一致に含まれる場合は、consしない。
	   (when (string-match rx name2)
	     (let ((match (cons (concat name (cdr (caar xs))) (cdar xs))))
	       (unless (or (member match names-search)
			   (member match complete))
		 (push match names-search))))))
       ;; 略称法令名検索
       (do ((xs (cdr (laws-names-alist)) (cdr xs))
	    (abbrev-search nil))
	   ((null xs) abbrev-search)
	 (when (string-match rx (caar xs))
	   ;; nil: closed flag
	   (push (cons (caar xs) (cons nil (cdar xs))) abbrev-search)))
       ;; 完全一致
       complete
       ;; 以前の検索結果の初期化。
       noclear)
      (message (concat (current-message) "done")))))

(defun laws-index-search-interactive ()
  (unless (file-exists-p laws-index-file)
    (error "Try `M-x laws'"))
  (when laws-setup-p (laws-setup))
  (let ((rx (read-string "Search: " nil 'laws-search-history)))
    (when (equal rx "")
      (error ""))
    (unless (eq major-mode 'laws-index-mode)
      (if (get-buffer laws-index-buffer)
	  (switch-to-buffer laws-index-buffer)
	(laws-index)))
    (when (assoc (format "検索式 `%s'" rx) laws-search-alist)
      (error "`%s' is retrieved." rx))
    (list rx current-prefix-arg)))

;; Searchモードで検索結果をハイライトする
(defun laws-index-highlight-search-buffer ()
  (labels ((put-overlay ()
	     (let ((rx (and (re-search-forward
			     "`\\(.+?\\)'" (line-end-position) t)
			    (match-string 1))))
	       ;; 検索式
	       (overlay-put
		(car (push (make-overlay (match-beginning 1)
					 (match-end 1))
			   laws-index-search-overlaies))
		'face '(:foreground "red"))
	       ;; 検索結果
	       (forward-line 1)
	       (while (and (/= (laws-index-folder-level) 0)
			   (not (eobp)))
		 (while (= (laws-index-folder-level) 1)
		   (forward-line 1))
		 (when (>= (laws-index-folder-level) 2)
		   (let* ((end (re-search-forward "[-+]\" \"\\(.+?\\)\""
						  (line-end-position) t))
			  (beg (and end (match-beginning 1))))
		     (when (and beg
				(string-match
				 rx (buffer-substring-no-properties beg end)))
		       (overlay-put
			(car (push (make-overlay (+ beg (match-beginning 0))
						 (+ beg (match-end 0)))
				   laws-index-search-overlaies))
			'face 'match)))
		   (forward-line 1)))
	       (and (= (laws-index-folder-level) 0)
		    (put-overlay)))))
    (mapc 'delete-overlay laws-index-search-overlaies)
    (setq laws-index-search-overlaies nil)
    (save-excursion
      (goto-char (point-min))
      (unless (eobp) (put-overlay)))))

;;
;; Bookmark
;;
(defun laws-index-bookmark-add ()
  "ブックマークに法令名を追加するコマンド。
ファイル:`laws-bookmark-file'に書き出す。"
  (interactive)
  (unless (eq laws-index-local-mode 'Bookmark)
    (condition-case err
	(destructuring-bind (flag name id) (laws-get-values)
	  (unless id (error "Not a law data."))
	  (let ((id (car id)))
	    (if (member id (laws-bookmark-alist))
		(message "Already exists in Bookmark.")
	      (push id laws-bookmark-alist)
	      (message "Add to Bookmark `%s'" name))))
      (error nil))))

(defun laws-index-put-deletion-flag ()
  "Bookmark,Opened,Recentで削除マークを付ける。"
  (interactive)
  (when (member laws-index-local-mode '(Opened Recent Bookmark))
    (laws-with-buffer-read-only
     (forward-line 0)
     (when (re-search-forward "\\([ D]\\)-" (line-end-position) t)
       (replace-match
	(string (+ (- ?D (string-to-char (match-string 1))) ?\ ))
	nil nil nil 1)))
    (forward-line 1)
    (when (eobp) (goto-char (point-min)))
    (laws-index-move-to-column)))

(defun laws-index-get-cells (&optional marks)
  "バッファからブックマークの各項目を取得して返す。
MARKSが非nilなら削除マークが付いた項目のみ。"
  (save-excursion
    (let ((result nil))
      (goto-char (point-min))
      (while (search-forward (if marks "\"D-" " -") nil t)
	(destructuring-bind (flag name id) (laws-get-values)
	  (push (car id) result)))
      (nreverse result))))

(defun laws-index-do-delete-marks ()
  "Bookmark,Opened,Recentで、削除マーク`D'が付いた項目を削除する。
Bookmarkの場合、ファイル:`laws-bookmark-file'に書き出す。
Openedの場合、ファイルを閉じる。"
  (interactive)
  (labels ((delalist (alist &optional form)
	     ;; ALIST is a symbol. Return a function.
	     `(lambda ()
		(mapc (lambda (cel)
			(setq ,alist (delete cel (funcall (function ,alist)))))
		      (or ,form (laws-index-get-cells 'marks))))))
    (case laws-index-local-mode
      (Bookmark
       (funcall
	(delalist 'laws-bookmark-alist
		  '(mapcar (lambda (x) (upcase x))
		    (laws-index-get-cells 'marks))))
       (laws-with-buffer-read-only (erase-buffer))
       (laws-index-insert-bookmark))
      (Opened
       (mapc (lambda (cel)
	       (kill-buffer
		(get-file-buffer (laws-expand-data-file cel))))
	     ;; mapc(delalist) returns it's arg identical.
	     (funcall (delalist 'laws-opened-alist)))
       (laws-with-buffer-read-only (erase-buffer))
       (laws-index-insert-opened))
      (Recent
       (funcall
	(delalist 'laws-recent-alist
		  '(mapcar (lambda (x) (upcase x))
		    (laws-index-get-cells 'marks))))
       (laws-with-buffer-read-only (erase-buffer))
       (laws-index-insert-recent)))))

(defun laws-index-bookmark-move-up ()
  "項目を1行上に移動する。"
  (interactive)
  (laws-index-bookmark-move-down t))

(defun laws-index-bookmark-move-down (&optional up)
  "項目を1行下に移動する。"
  (interactive)
  (when (eq laws-index-local-mode 'Bookmark)
    (laws-with-buffer-read-only
     (let* ((start (progn (forward-line 0) (point)))
	    (end   (progn (forward-line 1) (point)))
	    (line  (buffer-substring start end)))
       (delete-region start end)
       (forward-line (if up -1 1))
       (insert line)))
    (forward-line (if up -2 1))
    (when (eobp) (forward-line -1))
    (laws-index-move-to-column)
    (setq laws-bookmark-alist (laws-index-get-cells))))

;;
;; Scroll commands
;;
(defun laws-index-move-to-column ()
  "フォルダの開閉を表わすマーク位置に移動する関数。"
  (forward-line 0)
  (re-search-forward "[+-]" nil t)
  (ignore-errors (backward-char 1)))

(defun laws-index-previous-line (n)
  "前の行に移動するコマンド。"
  (interactive "p")
  (let ((p (point)))
    (if (> 0 (forward-line (- n)))
	(goto-char p)
      (laws-index-move-to-column))))

(defun laws-index-next-line (n)
  "次の行に移動するコマンド。"
  (interactive "p")
  (forward-line n)
  (when (eobp) (forward-line -1))
  (laws-index-move-to-column))

(defun laws-index-scroll-up-line (n)
  "N行前方にスクロールするコマンド。"
  (interactive "p")
  (ignore-errors
    (scroll-up n)
    (laws-index-move-to-column)))

(defun laws-index-scroll-down-line (n)
  "N行後方にスクロールするコマンド。"
  (interactive "p")
  (ignore-errors
    (scroll-down n)
    (laws-index-move-to-column)))

(defun laws-index-previous-folder ()
  "ポイントと同じレベルの前のフォルダに移動する。"
  (interactive)
  (laws-index-next-folder t))

(defun laws-index-next-folder (&optional previous)
  "ポイントと同じレベルの次のフォルダに移動する。"
  (interactive)
  (let ((func (laws-any-function
	       '(laws-index-folder-level-0
		 laws-index-folder-level-1
		 laws-index-folder-level-2
		 laws-index-folder-level-3)))
	(move-to (point)))
    (when (functionp func)
      (save-excursion
	(forward-line (if previous -1 1))
	(while (not (or (funcall func)
			(eobp)
			(bobp)))
	  (forward-line (if previous -1 1)))
	(when (funcall func)
	  (laws-index-move-to-column)
	  (setq move-to (point))))
      (goto-char move-to))))

(defun laws-completion-list (afunc)
  "補完リストを返す関数。AFUNCは連想リストを返す関数。"
  (do ((xs (funcall afunc) (cdr xs))
       (result nil (cons (caar xs) result)))
      ((null xs) result)))

(defun laws-index-goto-folder (folder)
  "補完リストから、最上位の階層に選択的に移動するコマンド。
`laws-use-iswitchb'がtなら`iswitchb'を利用する。"
  (interactive
   (list (funcall
	  (if laws-use-iswitchb
	      #'laws-icompleting-read
	    #'completing-read)
	  (if (memq laws-index-local-mode '(Index Directory Abbrev))
	      "Goto folder: " "Goto name: ")
	  (laws-completion-list
	   (cond ((eq laws-index-local-mode 'Index)     #'laws-index-alist)
		 ((eq laws-index-local-mode 'Directory) #'laws-directory-alist)
		 ((eq laws-index-local-mode 'Abbrev)    #'laws-abbrev-alist)
		 ((eq laws-index-local-mode 'Bookmark)
		  (lambda () (laws-make-alist-from-name #'laws-bookmark-alist)))
		 ((eq laws-index-local-mode 'Recent)
		  (lambda () (laws-make-alist-from-name #'laws-recent-alist)))
		 ((eq laws-index-local-mode 'Opened)    #'laws-opened-alist)
		 (t (error "Not supported.")))))))
  (unless (string= folder "")
    (goto-char
     (save-excursion
       (goto-char (point-min))
       (re-search-forward (format "[+-]\" \"%s\"" folder))))
    (laws-index-move-to-column)))

(defun laws-index-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min))
  (laws-index-move-to-column))

(defun laws-index-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (laws-index-move-to-column))

;;
;; laws index menu
;;
(easy-menu-define laws-index-mode-menu
    laws-index-mode-map
  "laws-index-menu"
  '("Laws"
    ["Open or Close Item"	laws-index-open-or-close t]
    ["Browse This URL"		laws-index-browse-at-point t]
    ["Search Law Name"		laws-index-search t]
    ["Laws Iswitchb"		laws-iswitchb t]
    "-"
    ["Update Buffer"		laws-index-update t]
    "-"
    ["Bookmark Add"		laws-index-bookmark-add t]
    ["Move Up Bookmark Item"	laws-index-bookmark-move-up t]
    ["Move Down Bookmark Item"	laws-index-bookmark-move-down t]
    "-"
    ["Put Deletion Mark"	laws-index-put-deletion-flag t]
    ["Delete Marked Items"	laws-index-do-delete-marks t]
    "-"
    ["Upper Level"		laws-index-upper-level t]
    ["Previous Same Level"	laws-index-previous-folder t]
    ["Next Same Level"		laws-index-next-folder t]
    ["Select Folder"		laws-index-goto-folder t]
    "-"
    ["Open All Folder"		laws-index-open-all t]
    ["Close All Folder"		laws-index-close-all t]
    "-"
    ["Previous Item"		laws-index-previous-line t]
    ["Next Item"		laws-index-next-line t]
    ["Scroll Up Line"		laws-index-scroll-up-line t]
    ["Scroll Down Line"		laws-index-scroll-down-line t]
;;     ["Beginning of Buffer"	laws-index-beginning-of-buffer t]
;;     ["End of Buffer"		laws-index-end-of-buffer t]
    "-"
    ["Help"			laws-index-help t]
    ("Other"
     ["Make Index Files"	laws-retrieve-index t]
     "-"
     ["Retrieve HTML Data"	 laws-retrieve-html t]
     ;;["法令ファイルの再作成"	 laws-menu-data-create t]
     )
    "-"
    ["Close Index"		bury-buffer t]
    ["Laws Exit"		laws-exit t]))

;;
;; Index mode
;;
;;;###autoload
(defalias 'laws 'laws-index)

;;;###autoload
(defun laws-index ()
  "法令データにアクセスするためのインターフェイス。"
  (interactive)
  (unless (file-exists-p laws-index-file)
    (unless laws-online-mode
      (error "Try `M-x laws-online-or-offline', and turn to online mode."))
    (laws-make-index-files))
  (switch-to-buffer
   (or (get-buffer laws-index-buffer)
       (prog1
	   (set-buffer (get-buffer-create laws-index-buffer))
	 (laws-index-mode)))))

(defun laws-index-mode ()
  "`laws-index'のためのメジャーモード。"
  (kill-all-local-variables)
  (use-local-map laws-index-mode-map)
  (setq mode-name laws-index-mode-name)
  (setq major-mode 'laws-index-mode)
  (set (make-local-variable 'laws-index-local-mode)
       laws-index-initial-mode)
  (set (make-local-variable 'laws-index-conf) nil)
  (set (make-local-variable 'laws-search-alist) nil)
  (set (make-local-variable 'laws-names-alist) nil)
  ;;(set (make-local-variable 'laws-search-history) nil)
  ;;(set (make-local-variable 'laws-alist) nil)
  ;;(set (make-local-variable 'laws-abbrev) nil)
  ;;(set (make-local-variable 'laws-abbrev-alist) nil)
  (set (make-local-variable 'laws-index-alist) nil)
  (set (make-local-variable 'laws-directory-alist) nil)
  (set (make-local-variable 'laws-index-search-overlaies) nil)
  (laws-index-goto-mode laws-index-initial-mode)
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults)
       '(laws-index-font-lock-keywords))
  (turn-on-font-lock)
  (setq truncate-lines t)
  (easy-menu-add laws-index-mode-menu)
  (and laws-mode-line
       (setq mode-line-buffer-identification laws-mode-line))
  (run-hooks 'laws-index-mode-hook)
  )

;; display icon on mode-line
(defun laws-online-or-offline ()
  "Turn on or off `laws-online-mode'."
  (interactive)
  (setq laws-online-mode (not laws-online-mode))
  (setq laws-icon
	(if laws-online-mode
	    laws-online-icon
	  laws-offline-icon))
  (force-mode-line-update)
  (laws-online-mode-message #'message))

(defun laws-online-mode-message (message-fun)
  (funcall message-fun (if laws-online-mode "On line mode." "Off line mode.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; laws
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;
;; search
;;
(defun laws-push-mouse-2 (e)
  (interactive "e")
  (mouse-set-point e)
  (call-interactively 'laws-search-or-push-anchor))

(defun laws-search-or-push-anchor (n)
  (interactive "p")
  (if current-prefix-arg
      ;; search
      (let ((kanji (laws-to-kanji-number n)))
	(when kanji
	  (goto-char
	   (save-excursion
	     (laws-move-to-article `(,(format "^第%s条" kanji)))))
	  (laws-winconf-add 'force)))
    ;; anchor push
    (laws-push-anchor)))

(defun laws-push-anchor (&optional new-window)
  (interactive "P")
  (cond ( ;; リージョンが活性の場合(罫線表内部で実行)
	 (and mark-active transient-mark-mode)
	 (laws-display-anchor
	  (laws-rectangle-anchor (region-beginning) (region-end))
	  new-window))
	( ;; 全角括弧
	 (eq (char-after) ?\（)
	 (laws-compose-paren-toggle))
	( ;; 通常のアンカーの場合
	 (memq (get-text-property (point) 'face)
	       '(laws-anchor-name-face
		 laws-anchor-article-face))
	 (laws-display-anchor (laws-anchor-at-point) new-window))
	( ;; アウトラインヘッダ
	 (laws-outline-header-p)
	 (laws-heading-jump))
	(t (princ "No anchor at point."))))

(defun laws-rectangle-anchor (start end)
  "罫線表の中の複数行に跨ったリージョンから法令名・条文番号等を文
字列で返す。"
  (save-excursion
    (goto-char start)
    (labels ((trim (anchor)
	       (let* ((kanji "[一二三四五六七八九十]")
		      (regexp (concat
			       "\\(第[一二三四五六七八九十百千]+条"
			       "\\(?:[のノ]"	kanji "+\\)*\\)"
			       "\\(第"		kanji "+項\\)*"
			       "\\(第"		kanji "+号"
			       "\\(?:[のノ]"	kanji "+\\)*\\)?")))
		 (if (string-match regexp anchor)
		     (substring anchor 0 (match-end 0))
		   anchor))))
      (let ((count-back 0)
	    (lines (count-lines start end))
	    (move-to
	     (save-excursion
	       (search-forward "│" (line-end-position) t)
	       (1- (current-column))))
	    back-to)
	(while (/= (preceding-char) ?│)
	  (backward-char)
	  (when (= (point) (line-beginning-position))
	    (error "Not a chart."))
	  (incf count-back))
	(setq back-to (current-column))
	;; 条数の後の文字列をトリミング
	(trim
	 (let (result)
	   (replace-regexp-in-string
	    "（.+?）" ""
	    (substring
	     (apply 'concat
		    (dotimes (x lines (nreverse result))
		      (push (buffer-substring-no-properties
			     (point)
			     (save-excursion
			       (move-to-column move-to)
			       (if (> (1- (point)) end)
				   end
				 (1- (point)))))
			    result)
		      (forward-line 1)
		      (move-to-column back-to)))
	     count-back))))))))

(defun laws-display-anchor (anchor new-window)
  (multiple-value-bind (name article paragraph item)
      (laws-parse-anchor anchor)
    (setq name
	  ;; If non nil, 法令名に変換。
	  (and name			;(laws-anchor-convert-to-ref name)
	       (laws-anchor-convert-entry-name name))
	  ;; If non nil, 正規表現文字列に変換。
	  article
	  (laws-anchor-article article)
	  ;; If non nil, 正規表現文字列に変換。
	  paragraph
	  (laws-anchor-paragraph paragraph)
	  ;; If non nil, 正規表現文字列に変換。
	  item
	  (laws-anchor-item item))
    (when (and (not new-window) (not (one-window-p)))
      (let ((lst (save-selected-window
		   (laws-other-window)
		   (list major-mode (laws-current-buffer-law-name)))))
	(if (eq (car lst) 'laws-mode)
	    (progn (and (null name)
			(setq name (cadr lst)))
		   (save-selected-window
		     (laws-other-window)
		     (delete-window)
		     (setq new-window t)))
	  (setq new-window t))))
    (unless name
      ;; If nil, 参照しているのは現在のファイル。
      (setq name (laws-current-buffer-law-name)))
    (laws-display (let* ((id (laws-get-id name))
			 (file (laws-expand-data-file id)))
		    (cond ((and name (eq id nil))
			   (error "Parse error: %S" (list name id)))
			  ((eq id nil)	; 未登録法令
			   (or (and (equal (laws-current-buffer-law-name)
					   name)
				    (buffer-file-name))
			       (error "Not visited file.")))
			  ((file-exists-p file) file)
			  (t (laws-make-data id))))
		  (list article paragraph item)
		  1)
    (sit-for 0.1)			;Test:
    (if (eq (laws-compare-winconf)
	    'different)
	(laws-winconf-add 'force)
      (princ '==))))

(defun laws-display (filename &optional search recenter select)
  "Return window."
  (let* ((buffer (laws-get-buffer filename))
	 (window (laws-split-window-vertically buffer)))
    (when search
      (save-selected-window
	(set-window-buffer (select-window window) buffer)
	(laws-move-to-article search recenter)))
    (when select (select-window window))
    window))

(defun laws-split-window-vertically (buffer &optional size)
  "Return the created window."
  (let ((window (split-window-vertically
		 (- (or size laws-window-height)))))
    (set-window-buffer window buffer)
    window))

(defun laws-set-window-height (height)
  (shrink-window (- (window-height) height 1)))

(defun laws-get-buffer (filename)
  "FILENAME is the path/filename."
  (or (get-file-buffer filename)
      (prog1
	  (set-buffer (find-file-noselect filename))
	(laws-mode))))

(defun laws-digit-argument-suffix (prefix)
  (interactive "p")
  (when (not current-prefix-arg)
    (error "/"))
  (let ((args (split-string
	       (concat (laws-digit-argument)) "-" t)))
    (mapc (lambda (x)
	    (when (zerop (string-to-number x))
	      (error "//")))
	  args)
    (goto-char
     (save-excursion
       (laws-move-to-article
	(laws-make-article-regexp prefix args))))
    (laws-winconf-add 'force)))

(defun laws-digit-argument (&optional events)
  (let ((ev (read-event)))
    (cond
      ((not (memq ev '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- 32 10 13 return)))
       (error "/"))
      ((memq ev '(32 10 13 return))
       (reverse events))
      (t
       (laws-digit-argument (cons ev events))))))

(defun laws-make-article-regexp (prefix args)
  (list
   (format "^第%s条%s"
	   (laws-to-kanji-number prefix)
	   (mapconcat (lambda (x)
			(format "[のノ]%s"
				(laws-to-kanji-number (string-to-number x))))
		      args ""))))

(defun laws-to-kanji-number (n)
  (labels ((split (n)
	     (if (zerop (/ n 10))
		 (list (% n 10))
	       (cons (% n 10) (split (/ n 10)))))
	   (kanji (n)
	     (nth n '("零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))))
    (apply #'concat
	   (nreverse
	    (laws:map (lambda (i s)
			(and (> i 0) (string= s "一") (setq s ""))
			(if (string= s "零") ""
			  (concat s (nth i '("" "十" "百" "千")))))
		      '(0 1 2 3) (mapcar #'kanji (split n)))))))

(defun laws-to-arabic-number (kanji &optional em-size)
  (if (string-match "[一二三四五六七八九十百千]+" kanji)
      (setq kanji (match-string 0 kanji))
    (error "Argument is not Chinese numeral"))
  (let* ((number "[一二三四五六七八九十]")
	 (regexp (concat
		  "\\(" number "?千\\)?"
		  "\\(" number "?百\\)?"
		  "\\(" number "?十\\)?"
		  "\\(" number "\\)?"))
	 (lst (progn
		;; 各桁の値のリストを生成
		(string-match regexp kanji)
		(mapcar (lambda (x) (match-string x kanji))
			'(1 2 3 4))))
	 result)
    (setq lst
	  ;; 各要素を数に変換
	  (dotimes (x 4 lst)
	    (let ((element (nth x lst)) p)
	      (and (or (equal element "千")
		       (equal element "百")
		       (equal element "十"))
		   (setq element (concat "一" element)))
	      (and element
		   (setq element (substring element 0 1)))
	      (setcar
	       (nthcdr x lst)
	       (cond
		 ((equal element '()) 0)
		 (t
		  (setq p (string-match element "一二三四五六七八九"))
		  (string-to-number
		   (substring "123456789" p (1+ p)))))))))
    ;; 合算
    (setq result
	  (apply (function +)
		 (mapcar (lambda (x)
			   (* (expt 10 (- 3 x)) (nth x lst)))
			 '(0 1 2 3))))
    ;; 全角文字に変換
    (or (and em-size
	     (japanese-zenkaku (number-to-string result)))
	result)))

;;
;; common
;;
(defun laws-recenter (&optional n)
  (interactive "p")
  (let ((line (cond ((numberp n) n)
		    ((null n) nil)
		    (t laws-reposition-line))))
    (when line
      ;;(sit-for 0)
      (recenter line))
    line))

(defun laws-scan-buffer (regexp direction count &optional recenter limit)
  (let ((found t))
    (labels ((scan (s)
                   (setq found
                         (case direction
                           (forward (and (bolp)
                                         (not (eobp))
                                         (forward-char))
                                    (re-search-forward s limit t))
                           (backward (unless (bolp) (forward-line 1))
                                     (re-search-backward s limit t))))
                   (when found (decf count))))
      (while (and found (> count 0))
	(scan regexp))
      (forward-line 0)
      (when found (laws-recenter recenter))
      found)))

;; (defun laws-current-article ()
;;   ;; ^$ ではなく、laws-article-regexp で判断するよう変更する。
;;   (let (beg end)
;;     (save-excursion
;;       (or (and (looking-at "^$")
;; 	       (setq end (point)
;; 		     beg (or (re-search-backward laws-article-regexp nil t)
;; 			     (point-min))))
;; 	  (setq end (or (re-search-forward "^$" nil t)
;; 			(point-max))
;; 		beg (or (re-search-backward laws-article-regexp nil t)
;; 			(point-min)))))
;;     (list beg end)))

;; (defun laws-current-article ()
;;   (let ((end-point
;; 	 (lambda ()
;; 	   (cond ((re-search-forward laws-article-regexp nil t)
;; 		  (while (not (looking-at "^$"))
;; 		    (forward-line -1))
;; 		  (when (>= beg (point))
;; 		    (error "Irregular article region."))
;; 		  (point))
;; 		 ((re-search-forward "^$" nil t)
;; 		  (point))
;; 		 (t (point-max)))))
;; 	beg end)
;;     (save-excursion
;;       (forward-line 0)
;;       (cond ((looking-at laws-article-regexp)
;; 	     (setq beg (point))
;; 	     (forward-line 1)
;; 	     (setq end (funcall end-point)))
;; 	    ((re-search-backward laws-article-regexp nil t)
;; 	     (setq beg (point))
;; 	     (forward-line 1)
;; 	     (setq end (funcall end-point)))
;; 	    (t (goto-char (point-min))
;; 	       (setq beg (point))
;; 	       (setq end (funcall end-point)))))
;;     (list beg end)))

(defun laws-current-article ()
  ;; Test:
  (labels ((end-point (p)
	     (cond ((re-search-forward
		     (concat
		      "\\(" laws-article-regexp "\\|" outline-regexp "\\)")
		     nil t)
		    (forward-line 0)
		    (or (looking-at outline-regexp)
			(while (not (looking-at "^$"))
			  (forward-line -1)))
		    (when (>= p (point))
		      (error "Irregular article boundaries."))
		    (point))
		   ((re-search-forward "^$" nil t)
		    (point))
		   (t (point-max)))))
    (let (beg end)
      (save-excursion
	(forward-line 0)
	(cond ((looking-at laws-article-regexp)
	       (setq beg (point))
	       (forward-line 1)
	       (setq end (end-point beg)))
	      ((re-search-backward laws-article-regexp nil t)
	       (setq beg (point))
	       (forward-line 1)
	       (setq end (end-point beg)))
	      (t (goto-char (point-min))
		 (setq beg (point))
		 (setq end (end-point beg))))
	(cons beg end)))))

;;
;; Move to article and paragraph
;;
(defun laws-move-to-article (args &optional recenter)
  (let ((article   (car  args))
	(paragraph (cadr args))
	(item      (nth 2  args)))
    (goto-char (point-min))
    (when article
      (if (re-search-forward article nil t)
	  (when (or paragraph item)
	    (let ((limit (cdr (laws-current-article))))
	      (mapc (lambda (rx)
		      (and rx (or (re-search-forward rx limit t)
				  (error "Not found."))))
		    `(,paragraph ,item))))
	(error "Not found.")))
    ;; found
    (forward-line 0)
    (laws-recenter (or recenter t))
    (point)))

(defun laws-forward-article (n)
  (interactive "p")
  ;; ポイントがアウトラインヘッダ上にあるかどうかで分岐
  ;; 同一階層間の移動
  (if (laws-outline-header-p)
      (laws-forward-same-level n)
    ;; 次の条文へ移動
    (or (laws-scan-buffer laws-article-regexp 'forward n t)
	(goto-char (point-max)))))

(defun laws-backward-article (n)
  (interactive "p")
  ;; ポイントがアウトラインヘッダ上にあるかどうかで分岐
  ;; 同一階層間の移動
  (if (laws-outline-header-p)
      (laws-backward-same-level n)
    ;; 前の条文へ移動
    (or (laws-scan-buffer laws-article-regexp 'backward n t)
	(goto-char (point-min)))))

(defun laws-forward-paragraph (n)
  (interactive "p")
  (save-selected-window
    (when (not (one-window-p))
      (select-window
       (cadr (memq (selected-window) (window-list)))))
    (if (eq major-mode 'laws-mode)
	(let ((limit (progn (when (looking-at "^$")
			      (error "/"))
			    (cdr (laws-current-article)))))
	  (or (laws-scan-buffer
	       laws-paragraph-regexp 'forward n nil limit)
	      (error "Last paragraph.")))
      (scroll-up))))

(defun laws-backward-paragraph (n)
  (interactive "p")
  (save-selected-window
    (when (not (one-window-p))
      (select-window
       (cadr (memq (selected-window) (window-list)))))
    (if (eq major-mode 'laws-mode)
	(let ((limit (let ((beg (car (laws-current-article))))
		       (and (< (point) beg)
			    (error "/"))
		       beg)))
	  (or (laws-scan-buffer
	       laws-paragraph-regexp 'backward n nil limit)
	      (error "First paragraph.")))
      (scroll-down))))

;;
;; Move to anchor
;;
(defun laws-forward-anchor ()
  (interactive)
  (laws-move-to-anchor 'forward))

(defun laws-backward-anchor ()
  (interactive)
  (laws-move-to-anchor 'backward))

;;
;; anchor
;;
(defun laws-move-to-anchor (direction)
  (labels ((point-face (&optional point)
	     (get-text-property (or point (point)) 'face))
	   (scanner ()
	     (case direction
	       (forward (next-property-change (point)))
	       (backward (previous-property-change (point))))))
    (let ((back-to (point)))
      (while (memq (point-face)
		   '(laws-anchor-article-face laws-anchor-name-face))
	(condition-case err
	    (goto-char (scanner))
	  (wrong-type-argument
	   (error "There is no anchor any further."))))
      (while (or (not (memq (point-face)
			    '(laws-anchor-article-face laws-anchor-name-face)))
		 (and (eq (char-before) ?\）)
		      (eq (point-face (- (scan-lists (point) -1 0) 1))
			  laws-anchor-name-face)))
	(condition-case err
	    (goto-char (scanner))
	  (wrong-type-argument
	   (goto-char back-to)
	   (error "There is no anchor any further."))))
      (when (eq (point-face (1- (point))) 'laws-anchor-name-face)
	(goto-char (previous-property-change (point)))))))

(defun laws-anchor-at-point ()
  (let ((anchor (laws-current-anchor)))
    (when anchor
      (replace-regexp-in-string
       "（.+?）" ""
       (buffer-substring-no-properties (car anchor) (cdr anchor))))))

(defun laws-parse-anchor (anchor)
  (let* ((kanji "[一二三四五六七八九十]")
	 (regexp (concat
		  "\\(第[一二三四五六七八九十百千]+条"
		  "\\(?:[のノ]"	kanji "+\\)*\\)"
		  "\\(第"	kanji "+項\\)*"
		  "\\(第"	kanji "+号"
		  "\\(?:[のノ]"	kanji "+\\)*\\)?\\'"))
	 ;; 条数
	 (article (and (string-match regexp anchor)
		       (match-string 1 anchor)))
	 ;; 項数
	 (paragraph (and (string-match regexp anchor)
			 (match-string 2 anchor)))
	 ;; 号数
	 (item (and (string-match regexp anchor)
		    (match-string 3 anchor)))
	 ;; 法令名
	 (name (or (and (not article)
			anchor)
		   (and (and (string-match article anchor)
			     (/= (match-beginning 0) 0))
			(substring anchor 0 (match-beginning 0))))))
    (list name article paragraph item)))

(defun laws-get-name (id)
  "ID(\"m29ho089\"のような形式)から法令名を取得して返す。"
  (block nil
    (let ((xs (laws-alist)))
      (while xs
	(let ((cell (rassoc id (cdar xs))))
	  (when cell
	    (return (caar cell))))
	(pop xs)))))

(defun laws-current-buffer-law-name ()
  "カレントファイルの法令名を返す。"
  (laws-get-name (upcase (laws-file-sans-name (buffer-file-name)))))

(defun laws-get-local-name (name id)
  (or (plist-get (cdr
		  (assoc (laws-get-name (upcase id)) laws-local-names-plist))
		 (intern (concat ":" name)))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward (concat "以下「" name "」という。") nil t)
	  (laws-backward-anchor)
	  (buffer-substring-no-properties
	   (point)
	   (goto-char (next-property-change (point))))))
      (error "Not entried.")))

(defun laws-get-id (name)
  "法令名NAMEから参照先を返す。"
  (block nil
    ;; 登録法令名
    (let ((xs (laws-alist)))
      (while xs
	(let ((ys (cdar xs)))
	  (while ys
	    (let ((cell (car ys)))
	      (when (string= (caar cell) name)
		(return (cdr cell))))
	    (pop ys)))
	(pop xs)))
    ;; 略称法令名
    (let ((xs (laws-abbrev)))
      (while xs
	(let ((ys (cdar xs)))
	  (while ys
	    (let ((cell (cdr (assoc (format "「%s」" name) ys))))
	      (when cell
		(if (= (length cell) 1)
		    (return (cdar cell))
		  ;; 略称法令名で、ひとつの略称に対して複数の法令が対応してい
		  ;; る場合に補完リストから選択する。
		  (let* ((selected (completing-read "Select: " cell nil t))
			 (id (cdr (assoc selected cell))))
		    (if id
			(return id)
		      (error "Cancel."))))))
	    (pop ys)))
	(pop xs)))))

(defun laws-anchor-convert-entry-name (name)
  ;; nameに対応する法令ファイル名を返す。
  (cond ((member name laws-local-name-list)
	 ;; 「法」「令」「規則」「新法」「旧法」等、具体的な法令名に変換する。
	 (laws-anchor-convert-entry-name
	  (laws-get-local-name name (laws-file-sans-name (buffer-file-name)))))
	;; 「附則」への対応(法令を開くのみ)。
	;; `laws-anchor-at-point'で条数、項数等は取得しない。
	((string= name "同法附則")
	 (error "Not supported."))
	((string-match "^\\(.+?\\)附則$" name)
	 (match-string 1 name))
	;; 「同法」
	((string= name "同法")
	 (save-excursion
	   (goto-char (next-property-change (point)))
	   (dotimes (x 1) (laws-move-to-anchor 'backward))
	   (while (or (not (eq (get-text-property (point) 'face)
			       laws-anchor-name-face))
		      (looking-at "同法"))
	     (laws-move-to-anchor 'backward))
	   (laws-anchor-convert-entry-name
	    (car (laws-parse-anchor (laws-anchor-at-point))))))
	;; 「憲法」等、登録名称に変換。
	((assoc name laws-unentry-names)
	 (laws-anchor-convert-entry-name
	  (cdr (assoc name laws-unentry-names))))
	;; 未施行法令の場合w3mでdumpする。
	((assoc name laws-mishikou-list)
	 (unless laws-online-mode
	   (laws-online-mode-message #'error))
	 (shell-command
	  (concat ;;(funcall laws-w3m-dump-command laws-w3m-dump-cols)
		  "w3m -dump "
		  (format " %s%s%s"
			  laws-egov-url
			  laws-htmldata-directory
			  (cdr (assoc name laws-mishikou-list)))))
	 (error "/"))
	;; 登録法令名、略称法令名
	(t name)))

(defun laws-anchor-article (article)
  (when article
    (setq article
	  (replace-regexp-in-string "[のノ]" "[のノ]" article))
    (concat "^" article "[ 　]*")))

(defun laws-anchor-paragraph (paragraph)
  (when paragraph
    (setq paragraph
	  (laws-to-arabic-number paragraph 'em-size))
    (and (not (string= "１" paragraph))
	 (format "^\\(%s\\|[○◯]%s\\)" paragraph paragraph))))

(defun laws-anchor-item (item)
  (and item
       (string-match "\
第\\([一二三四五六七八九十]+\\)号\\(\\([のノ][一二三四五六七八九十]+\\)*\\)" item)
       (concat "^" (match-string 1 item) (match-string 2 item))))



(defun laws-file-sans-name (file)
  "ファイル名の主部(m29ho089)を返す。"
  (when file
    (file-name-nondirectory (file-name-sans-extension file))))

(defun laws-buffer-file-laws-name (&optional full)
  "buffer-file-nameからdirectoryとextentionを除いた法令名を返す。
FULL が非-nilなら path/file を返す。"
  (let ((visitp (buffer-file-name)))
    (or (and (not visitp)
	     (error "Buffer not visiting any file."))
	(or (and full visitp)
	    (laws-file-sans-name (buffer-file-name))))))

(defun laws-rename-buffer ()
  "`laws-use-buffer-law-name'が非nilなら、法令ファイルのバッファ名
を法令名とする。法令名の長さが`laws-name-length'より大きければ、
`laws-name-suffix'を付加してバッファ名を縮小する。"
  (let* ((id (laws-file-sans-name (buffer-file-name)))
	 (name (laws-get-name (upcase id))))
    (when (and name laws-use-buffer-law-name)
      (when (> (length name) laws-name-length)
	(setq name
	      (concat (substring name 0 laws-name-length)
		      laws-name-suffix)))
      (rename-buffer name 'uniq))))

(defun laws-display-toggle ()
  (interactive)
  (if (and (one-window-p) laws-display-toggle-winconf)
      ;; restore
      (let ((buffer (current-buffer))
	    (winstart (window-start))
	    (pos (point)))
	(laws-restore-winconf laws-display-toggle-winconf)
	(set-window-buffer (selected-window) buffer)
	(set-window-start (selected-window) winstart)
	(goto-char pos))
    ;; store
    (when (not (one-window-p))
      (setq laws-display-toggle-winconf (laws-current-winconf))
      (delete-other-windows))))

;;
;; Window change
;;
(defun laws-other-window (&optional window)
  (interactive)
  (when (not (one-window-p))
    (select-window
     (or window
	 (cadr (memq (selected-window) (window-list)))))))

;;
;; Scroll
;;
(defun laws-scroll-up-screen (n)
  (interactive "p")
  (scroll-up n))

(defun laws-scroll-down-screen (n)
  (interactive "p")
  (scroll-down n))

;;
;; other
;;
(defun laws-browse-current-url ()
  (interactive)
  (let ((url (laws-expand-htmldata-url
	      (laws-file-sans-name (buffer-file-name)))))
    (if (not (string= url ""))
	(browse-url url)
      (message "No url in this file."))))

(defun laws-view-quit ()
  (interactive)
  (bury-buffer)
  (laws-index))

;;
;; window configuration
;;
(defalias 'laws-current-winconf 'current-window-configuration-printable)
(defalias 'laws-restore-winconf 'restore-window-configuration)

;; (defalias 'laws-current-winconf 'current-window-configuration)
;; (defalias 'laws-restore-winconf 'set-window-configuration)

(defun laws-winconf-override ()
  (interactive)
  (if (and laws-winconf-list
	   (not (laws-winconf-equalp))
	   (y-or-n-p (format "%s Override winconf? "
			     (progn (laws-winconf-message)
				    (current-message)))))
      (progn
	(setcar (nthcdr laws-winconf-index laws-winconf-list)
		(laws-current-winconf))
	(laws-winconf-message '=))
    (laws-winconf-message)))

(defun laws-winconf-insert ()
  (interactive)
  (if (and laws-winconf-list
	   (not (laws-winconf-equalp))
	   (y-or-n-p "Insert winconf? "))
      (progn
	(push (laws-current-winconf)
	      (nthcdr laws-winconf-index laws-winconf-list))
	(when (/= laws-winconf-index 0)
	  (setq laws-winconf-index (- laws-winconf-index 1)))
	(laws-winconf-message '+))
    (laws-winconf-message)))

(defun laws-winconf-add (&optional force)
  (interactive)
  (if (and (not (laws-winconf-equalp))
	   (or force (y-or-n-p "Add winconf? ")))
      (progn
	(push (laws-current-winconf)
	      (nthcdr (+ laws-winconf-index
			 (or (and (= (length laws-winconf-list) 0) 0)
			     1))
		      laws-winconf-list))
	(setq laws-winconf-index
	      (+ laws-winconf-index
		 (or (and (= (length laws-winconf-list) 1) 0)
		     1)))
	(laws-winconf-message '+))
    (laws-winconf-message)))

(defun laws-winconf-delete ()
  (interactive)
  (if (and laws-winconf-list
	   (y-or-n-p "Delete winconf? "))
      (progn
	(setf (nthcdr laws-winconf-index laws-winconf-list)
	      (nthcdr (+ laws-winconf-index 1) laws-winconf-list))
	(when (and (= (length laws-winconf-list) laws-winconf-index)
		   (/= laws-winconf-index 0))
	  (setq laws-winconf-index (- laws-winconf-index 1)))
	(laws-winconf-message '-))
    (laws-winconf-message)))

(defun laws-winconf-delete-all ()
  (interactive)
  (if (and laws-winconf-list
	   (y-or-n-p "Delete all winconf? "))
      (progn (setq laws-winconf-list nil
		   laws-winconf-index 0)
	     (princ 'Done))
    (laws-winconf-message)))

(defun laws-winconf-backward-delete ()
  (interactive)
  (if (and laws-winconf-list
	   (/= laws-winconf-index 0)
	   (y-or-n-p "Delete backward winconf? "))
      (progn
	(setf (nthcdr (- laws-winconf-index 1) laws-winconf-list)
	      (nthcdr laws-winconf-index laws-winconf-list))
	(setq laws-winconf-index (- laws-winconf-index 1))
	(laws-winconf-message '-))
    (laws-winconf-message)))

(defun laws-forward-winconf ()
  (interactive)
  (if laws-winconf-list
      (if (= (- (length laws-winconf-list) 1)
	     laws-winconf-index)
	  (laws-winconf-message)
	(laws-restore-winconf
	 (nth (+ laws-winconf-index 1) laws-winconf-list))
	(setq laws-winconf-index (+ laws-winconf-index 1))
	(laws-winconf-message))
    (laws-winconf-message)))

(defun laws-backward-winconf ()
  (interactive)
  (if laws-winconf-list
      (if (= laws-winconf-index 0)
	  (laws-winconf-message)
	(laws-restore-winconf
	 (nth (- laws-winconf-index 1) laws-winconf-list))
	(setq laws-winconf-index (- laws-winconf-index 1))
	(laws-winconf-message))
    (laws-winconf-message)))

(defun laws-restore-current-winconf ()
  (interactive)
  (if laws-winconf-list
      (progn (laws-restore-winconf
	      (nth laws-winconf-index laws-winconf-list))
	     (laws-winconf-message '=))
    (laws-winconf-message)))

(defun laws-restore-first-winconf ()
  (interactive)
  (if laws-winconf-list
      (progn (laws-restore-winconf (nth 0 laws-winconf-list))
	     (setq laws-winconf-index 0)
	     (laws-winconf-message))
    (laws-winconf-message)))

(defun laws-restore-last-winconf ()
  (interactive)
  (if laws-winconf-list
      (progn (laws-restore-winconf
	      (nth (- (length laws-winconf-list) 1)
		   laws-winconf-list))
	     (setq laws-winconf-index (- (length laws-winconf-list) 1))
	     (laws-winconf-message))
    (laws-winconf-message)))

(defun laws-restore-nth-winconf (n)
  (when (and laws-winconf-list
	     (<= 0 n) (< n (length laws-winconf-list)))
    (laws-restore-winconf (nth n laws-winconf-list))
    (setq laws-winconf-index n)
    (laws-winconf-message)))

(defun laws-winconf-equalp ()
  (when (equal
	 (nth laws-winconf-index laws-winconf-list)
	 (laws-current-winconf))
    (message "==")
    (sit-for 0.5)))

(defun laws-current-anchor ()
  (if (and mark-active transient-mark-mode)
      ;; 罫線表内でリージョンが活性の場合への対応
      (progn
	(deactivate-mark)
	(cons (region-beginning) (region-end)))
    ;; 通常のアンカーの場合
    (labels ((point-face (p) (get-text-property p 'face))
	     (next (p) (goto-char (next-property-change p))))
      (when (memq (point-face (point))
		  '(laws-anchor-name-face laws-anchor-article-face))
	(let ((back-to (point))
	      (face (point-face (point)))
	      start end)
	  (save-excursion
	    (setq end (next back-to))
	    (cond ((and (= (following-char) ?\（)
			(eq face 'laws-anchor-name-face))
		   ;; ○○法（○○○）第○○条第○号
		   (forward-sexp)
		   (and (eq (point-face (point)) 'laws-anchor-article-face)
			(setq end (next (point)))))
		  ((eq (point-face (point)) 'laws-anchor-article-face)
		   ;; ○○法第○○条第○号
		   (setq end (next (point)))))
	    (laws-move-to-anchor 'backward)
	    (setq start (point)))
	  (cons start end))))))

(defun laws-compare-winconf ()
  (let ((stored (nth laws-winconf-index laws-winconf-list))
	(current (laws-current-winconf)))
    (cond
      ((and (equal (nth laws-winconf-index laws-winconf-list)
		   (laws-current-winconf))
	    'identical))
      ((and (/= (length (nth 3 stored))
		(length (nth 3 current)))
	    'different))
      (t
       (let ((diffs (laws-winconf-diffs stored current)))
	 (or (and (> (length diffs) 1)
		  'different)
	     (let* ((buf (member (buffer-name) (car diffs)))
		    ;; points: (current-pos (point))
		    (points (let (ret)
			      (dolist (x (car diffs) ret)
				(and (integerp x)
				     (push x ret)))))
		    (pos (car (memq (point) points))))
	       (or (or (and (not buf)
			    'different)
		       (and (not pos)
			    'different))
                   (save-excursion
                     (let ((anchor (laws-current-anchor)))
                       (or (and (>= (cadr points) (car anchor))
                                (<= (cadr points) (cdr anchor))
                                (>= (car  points) (car anchor))
                                (<= (car  points) (cdr anchor))
                                'identical)
                           'different)))))))))))

(defun laws-winconf-diffs (stored current)
  (let ((sbuflst (nth 3 stored))
	(cbuflst (nth 3 current))
	sbuf cbuf sbuffer cbuffer swinst cwinst spos cpos diffs)
    (catch 'compare
      (while sbuflst
	(setq sbuf    (car sbuflst)
	      cbuf    (car cbuflst)
	      sbuffer (revive:get-buffer sbuf)
	      cbuffer (revive:get-buffer cbuf))
	(and (not (equal sbuffer cbuffer))
	     (throw 'compare 'different))
	(setq swinst (revive:get-window-start sbuf)
	      cwinst (revive:get-window-start cbuf)
	      spos   (revive:get-point sbuf)
	      cpos   (revive:get-point cbuf))
	(and (/= swinst cwinst)
	     (throw 'compare 'different))
	(and (/= spos cpos)
	     (push (list cbuffer cpos sbuffer spos) diffs))
	(setq sbuflst (cdr sbuflst)
	      cbuflst (cdr cbuflst))))
    diffs))

(defun laws-winconf-message (&optional arg)
  (interactive)
  (message
   (if laws-winconf-list
       (format "%s[%d/%d]"
	       (if arg (concat "(" (symbol-name arg) ")") "")
	       (+ laws-winconf-index
		  (if laws-winconf-list
		      1
		    0))
	       (length laws-winconf-list))
     "Not stored.")))

;;
;; parenthesis
;;
(defun laws-forward-paren (arg)
  (interactive "P")
  (cond
   (current-prefix-arg
    (laws-backward-paren))
   ((eq (char-after) ?\（)
    (forward-list)
    (laws-forward-paren nil))
   ((re-search-forward ".（" nil t)
    (backward-char))))

(defun laws-backward-paren ()
  (and (re-search-backward ".（" nil t)
       (forward-char)))

(defun laws-fontify-or-defontify-paren ()
  (interactive)
  (destructuring-bind (beg . end) (laws-current-article)
    (or (let ((pos (and laws-paren-overlays
			(overlay-start (car laws-paren-overlays)))))
	  ;; overlayが設定済みか
	  ;; overlayの位置が現在の条文内か
	  (and pos
	       (< beg pos)
	       (< pos end)
	       ;; defontify
	       (laws-defontify-paren)))
	(or (and laws-paren-overlays
		 ;; 他の条項でoverlayが設定済み
		 ;; defontify other
		 (laws-defontify-paren)
		 ;; fontify
		 (laws-fontify-paren))
	    ;; 通常のケース
	    ;; fontify
	    (laws-fontify-paren)))))

(defun laws-fontify-paren (&optional beg end)
  (interactive)
  (or (and beg end)
      (destructuring-bind (bg . ed) (laws-current-article)
	(setq beg bg end ed)))
  (let ((paren (laws-matching-parens beg end)))
    (save-excursion
      (mapc (lambda (x)
	      (laws-paren-overlay-put (goto-char (car x)) (cdr x)))
	    paren))))

(defun laws-defontify-paren ()
  (interactive)
  (when laws-paren-overlays
    (mapc #'delete-overlay laws-paren-overlays)))

(defun laws-paren-overlay-put (beg end)
  (or (looking-at laws-paren-exclude-regexp)
;      (looking-at "（[^）]*?」")
      (let ((overlays '(laws-paren1-face
			laws-paren2-face
			laws-paren3-face
			laws-paren4-face
			laws-paren5-face
			laws-paren6-face)))
	(let ((ov (memq (get-char-property (point) 'face)
			overlays)))
	  (overlay-put (car (push (make-overlay beg end)
				  laws-paren-overlays))
		       'face
		       (or (and ov
				(or (and (eq (car ov)
					     (last overlays))
					 (last overlays))
				    (cadr ov)))
			   (car overlays)))))))

(defun laws-matching-parens (begin end &optional outside-only)
  ;; Invalid search bound (wrong side of point)
  (save-excursion
    (goto-char begin)
    (let (parens)
      (while (re-search-forward laws-parens end t)
	(push (cons (match-beginning 0)
		    (scan-lists (match-beginning 0) 1 0)) parens)
	(goto-char (or (and outside-only
			    (scan-lists (match-beginning 0) 1 0))
		       (+ (match-beginning 0) 1))))
      (nreverse parens))))

(defun laws-compose-paren-toggle ()
  (interactive)
  (let ((end (cadr (get-text-property (point) 'composition)))
	(beg (point)))
    (and (eq (char-after) ?\（)
	 (if end
	     (decompose-region beg (+ beg end))
	   (laws-compose-paren (point) (scan-lists (point) 1 0))))))

(defun laws-compose-paren (&optional beg end)
  (interactive)
  (or (and beg end)
      (destructuring-bind (bg . ed) (laws-current-article)
	(setq beg bg end ed)))
  (let ((parens (laws-matching-parens beg end 'outside-only)))
    (save-excursion
      (mapc (lambda (x)
	      (laws-compose-region (goto-char (car x)) (cdr x)))
	    parens))))

(defun laws-compose-region (beg end)
  (or (looking-at laws-paren-exclude-regexp)
      (compose-region beg end laws-compose-paren-char)))

(defun laws-decompose-paren ()
  (interactive)
  (destructuring-bind (beg . end) (laws-current-article)
    (and current-prefix-arg
	 (setq beg (point-min) end (point-max)))
    (decompose-region beg end)))

;;
;; information
;;

;; (defun laws-display-file-info ()
;;   (interactive)
;;   (pop-to-buffer
;;    (with-current-buffer (get-buffer-create "*LawsInfo*")
;;      (laws-with-buffer-read-only
;;       (insert
;;        (apply #'format
;; 	      (mapconcat (lambda (s) (format "%-20s%%s" s))
;; 			 '("法令名" "URL" "HTML" "ファイル名" "日付") "\n")
;; 	      (laws-file-info (buffer-file-name)))))
;;      (current-buffer))))

;; (defun laws-file-info (file)
;;   (let ((data (laws-read-init-file file)))
;;     (let ((name (plist-get data :name))
;; 	  (url  (plist-get data :url))
;; 	  (html (plist-get data :html))
;; 	  (file (plist-get data :file))
;; 	  (date (plist-get data :date)))
;;       (list name url html file date))))

(defun laws-describe-bindings (keymap)
  (let ((name (symbol-name keymap)))
    (help-setup-xref (list #'laws-describe-bindings keymap)
                     nil)
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (princ (substitute-command-keys (concat "\\{" name "}"))))))

(defun laws-help ()
  (interactive)
  (laws-describe-bindings 'laws-mode-map))

(defun laws-index-help ()
  (interactive)
  (laws-describe-bindings 'laws-index-mode-map))

;;
;; outline
;;
(defun laws-outline-regexps ()
  (let ((number "[一二三四五六七八九十]"))
    (setq outline-regexp
	  (concat ;; 目次
	   ;; 目次のアウトラインヘッダが行頭にある場合(廃止法令等一覧)
	   ;; がある。
	   "^"
	   "\\(　\\{2\\}第" number "+編"
	   "\\|　\\{3\\}第" number "+章"
	   "\\|　\\{4\\}第" number "+節"
	   "\\|　\\{5\\}第" number "+款"
	   "\\|　\\{6\\}第" number "+目"
	   "\\|　\\{3\\}附　則"
	   ;; 本文
	   ;; 本文のアウトラインヘッダは、行頭から始まってい
	   ;; ないこと。
	   "\\|第"     number "+編　"
	   "\\|\\(第"  number "+章"
	   "\\([のノ]" number "\\)*\\)　"
	   "\\|\\(第"  number "+節"
	   "\\([のノ]" number "\\)*\\)　"
	   "\\|\\(第"  number "+款"
	   "\\([のノ]" number "\\)*\\)　"
	   "\\|\\(第"  number "+目"
	   "\\([のノ]" number "\\)*\\)　"
	   "\\|附則　?.*$\\)")))
  (setq outline-level 'laws-outline-level))

(defun laws-outline-level ()
  (save-excursion
    (let ((str (if (looking-at outline-regexp) (match-string 1) ""))
	  (number "[一二三四五六七八九十]"))
      (cond
       ((string-match (concat "第" number "+編") str) 1)
       ((string-match (concat "第" number "+章") str) 2)
       ((string-match (concat "第" number "+節") str) 3)
       ((string-match (concat "第" number "+款") str) 4)
       ((string-match (concat "第" number "+目") str) 5)
       ((string-match "附　?則" str) laws-supplementary-level)
       (t 0)))))

(defun laws-outline-forward-same-level (n)
  ;; from outline.el
  (outline-back-to-heading)
  (catch 'loop
    (while (> n 0)
      (let ((point-to-move-to (save-excursion
				(outline-get-next-sibling))))
	(if point-to-move-to
	    (progn
	      (goto-char point-to-move-to)
	      (setq n (1- n)))
	  (throw 'loop
		 (prog1 t
		   (message "No following same-level heading."))))))))

(defun laws-outline-up-heading (n)
  ;; from outline.el
  (outline-back-to-heading)
  (if (eq (funcall outline-level) 1)
      (error "Already at top level of the outline."))
  (while (and (> (funcall outline-level) 1)
	      (> n 0)
	      (not (bobp)))
    (let ((present-level (funcall outline-level)))
      (while (and (not (< (funcall outline-level) present-level))
		  (not (bobp)))
	(outline-previous-visible-heading 1))
      (setq n (- n 1)))))

(defun laws-outline-header-p ()
  (memq (get-text-property (point) 'face)
	'(laws-volume-face
	  laws-chapter-face
	  laws-section-face
	  laws-subsection-face
	  laws-subsection2-face)))

(defun laws-previous-visible-heading (n)
  (interactive "p")
  (forward-line 0)
  (if (and
       (laws-heading-level)
       (not (looking-at outline-regexp)))
      (progn
	(forward-line -1)
	(and (looking-at "^$")
	     ;; return if blank line
	     (forward-line)
	     (error "No previous heading.")))
    (outline-previous-visible-heading n)
    (laws-recenter t)))

(defun laws-next-visible-heading (n)
  (interactive "p")
  (forward-line 0)
  (if (and
       (laws-heading-level)
       (not (looking-at outline-regexp)))
      (progn
	(forward-line)
	(and (looking-at "^$")
	     ;; return if blank line
	     (forward-line -1)
	     (error "No following heading.")))
    (outline-next-visible-heading n)
    (laws-recenter t)))

(defun laws-forward-same-level (n)
  (interactive "p")
  ;; when laws-heading-level
  (and (laws-heading-level)
       (if (looking-at outline-regexp)
	   ;; outline heading
	   (progn (outline-forward-same-level n)
		  (laws-recenter t))
	 ;; not outline heading
	 (if (save-excursion (forward-line)
			     (looking-at "^$"))
	     ;; blank line
	     (error "No following same-level heading.")
	   ;; not blank line
	   (let ((pt (point)))
	     ;; error if forward-line is laws-supplementary-level
	     (if (and (/= (car (laws-heading-level))
			  laws-supplementary-level)
		      (save-excursion (forward-line)
				      (= (car (laws-heading-level))
					 laws-supplementary-level)))
		 (error "No following same-level heading.")
	       (condition-case err
		   (laws-forward-same-level-2)
		 (error
		  ;; Return to the position, if no following same level.
		  (goto-char pt)
		  (error "No following same-level heading.")))
	       ;; Returns to the position, if point is an outline header.
	       (when (looking-at outline-regexp)
		 (goto-char pt)
		 (error "No following same-level heading."))))))))

(defun laws-forward-same-level-2 ()
  (let ((level (car (laws-heading-level)))
	(pt (point)))
    (laws-next-visible-heading 1)
    (if (> level (car (laws-heading-level)))
	(progn
	  (laws-previous-visible-heading 1)
	  (error "No following same-level heading."))
      (while (< level (car (laws-heading-level)))
	(laws-next-visible-heading 1))
      (and
       (> level (car (laws-heading-level)))
       (goto-char pt)
       (error "No following same-level heading.")))))

(defun laws-backward-same-level (n)
  (interactive "p")
  (and
   (laws-heading-level)
   (if (looking-at outline-regexp)
       (progn
	 (outline-backward-same-level n)
	 (laws-recenter t))
     (if (save-excursion (forward-line -1)
			 (not (laws-heading-level)))
	 (error "No previous same-level heading.")
       (laws-backward-same-level-2)))))

(defun laws-backward-same-level-2 ()
  (let ((level (car (laws-heading-level))))
    (laws-previous-visible-heading 1)
    (if (> level (car (laws-heading-level)))
	(progn
	  (laws-next-visible-heading 1)
	  (error "No previous same-level heading."))
      (while (< level (car (laws-heading-level)))
	(laws-previous-visible-heading 1)))))

(defun laws-up-heading (n)
  (interactive "p")
  (if (laws-heading-level)
      (if (or ;; top-level or second-level(but not exists top-level)
	      (eq (car (funcall 'laws-heading-level)) 1)
		  (and (eq (car (funcall 'laws-heading-level)) 2)
		       (not (re-search-backward
			     "^　+第[一二三四五六七八九十]+編" nil t))))
	  (error "Already at top level of the outline.")
	(if (= 0 (laws-outline-level))
	    (laws-up-heading-2)
	  (condition-case err
	      (outline-up-heading n)
	    (error nil))
	  (laws-recenter t)))
    (outline-back-to-heading)
    (laws-recenter t)))

(defun laws-up-heading-2 ()
  (if (looking-at "^　+第[一二三四五六七八九]+編")
      (error "")
  (let ((level (car (laws-heading-level))))
    (while (<= level (car (laws-heading-level)))
      (laws-previous-visible-heading 1)))))

(defun laws-heading-level ()
  (save-excursion
    (let ((str (if (looking-at laws-heading-regexp) (match-string 1) ""))
	  (number "[一二三四五六七八九十]"))
      (cond
       ((string-match (concat "第" number "+編") str)
	(cons 1 (match-string 0 str)))
       ((string-match
	 (concat "第" number "+章\\([のノ]" number "\\)*") str)
	(cons 2 (match-string 0 str)))
       ((string-match
	 (concat "第" number "+節\\([のノ]" number "\\)*") str)
	(cons 3 (match-string 0 str)))
       ((string-match
	 (concat "第" number "+款\\([のノ]" number "\\)*") str)
	(cons 4 (match-string 0 str)))
       ((string-match
	 (concat "第" number "+目\\([のノ]" number "\\)*") str)
	(cons 5 (match-string 0 str)))
       ((string-match "附　?則" str)
	(cons laws-supplementary-level (match-string 0 str)))
       (t nil)))))

(defun laws-heading-list ()
  (let* ((heading (laws-heading-level))
	 (level (car heading))
	 (header-string-list (cons (cdr heading) '())))
    (cond
     ((null heading)
      (error "Not header line point."))
     ((= level 1) nil)			 ; 編
     ((= laws-supplementary-level level) ; 附則
      (setq header-string-list (list "附　?則")))
     (t
      (catch 'loop
	(save-excursion
	  (while (< 0 level)
	    (if (re-search-backward laws-heading-regexp nil t)
		(when (= (1- level) (car (laws-heading-level)))
		  (setq level (1- level)
			header-string-list (cons (cdr (laws-heading-level))
						 header-string-list)))
	      (throw 'loop nil)))))))
    header-string-list))

(defun laws-heading-jump ()
  (interactive)
  (forward-line 0)
  (let ((lst (laws-heading-list)))
    (when lst
      (and
       (looking-at outline-regexp)
       (goto-char (point-min)))
      (forward-char)
      (while lst
	(re-search-forward
	 (concat "^　*" (car lst)
		 (if (string= "附　?則" (car lst)) "" "　")) nil t)
	(pop lst))
      (forward-line 0)
      (and
       (looking-at outline-regexp)
       (laws-recenter t)))))

(defun laws-goto-toc ()
  "目次に移動するコマンド。"
  (interactive)
  (let ((pt (save-excursion
	      (goto-char (point-min))
	      (re-search-forward "^　+第一[編章]" nil t))))
    (if (not pt)
	(message "No table of contents found.")
      (goto-char pt)
      (forward-line 0)
      (recenter 1))))

(defun laws-move-to-tables ()
  "罫線表に移動するコマンド。"
  (interactive)
  (let ((backto (point)))
    (forward-char)
    (if (re-search-forward "^┌" nil t)
	(forward-line 0)
      (goto-char backto)
      (error "No chart found."))))

;;
;; Bookmark
;;
(defun laws-bookmark-this-file ()
  (interactive)
  (let ((id (laws-get-id (laws-current-buffer-law-name))))
    (if (member id (laws-bookmark-alist))
	(message "Already exists in Bookmark.")
      (push id laws-bookmark-alist)
      (message "Add to Bookmark `%s'" (laws-current-buffer-law-name)))))

;;
;; iswitchb
;;
(defun laws-icompleting-read (prompt choices)
  ;; See iswitchb.el commentary.
  (let ((minibuffer-setup-hook 'iswitchb-minibuffer-setup)
	(iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices)))
	(max-mini-window-height laws-max-mini-window-height))
    (iswitchb-read-buffer prompt)))

(defun laws-iswitchb (subject)
  "iswitchbで法令ファイルを開く。
ローマ字によるインクリメンタル検索(migemo)を利用するため、
migemoとiswitchbの設定が必要。"
  (interactive (laws-iswitchb-interactive))
  (message "Reading names...")
  (let ((name (funcall
	       (if laws-use-iswitchb
		   #'laws-icompleting-read
		 #'completing-read)
	       (format "[%S] Switch to: " subject)
	       (case subject
		 (all		(laws-names-list))
		 (bookmark	(laws-iswitchb-bookmark-list))
		 (download	(laws-iswitchb-download-list))
		 (t		(error "error: %S" subject))))))
    (laws-open-file (or (laws-get-id name) (error "No match.")))))

(defun laws-iswitchb-interactive ()
  (unless (file-exists-p laws-index-file)
    (error "Try `M-x laws'"))
  (when laws-setup-p (laws-setup))
  (list (if current-prefix-arg
	    (setq laws-iswitchb-present-list
		  (intern
		   (let* ((collection '("all" "bookmark" "download"))
			  (subject
			   (completing-read "Select a list: "
					    collection nil t)))
		     (or (car (member subject collection))
			 laws-iswitchb-present-list
			 'all))))
	  (or laws-iswitchb-present-list laws-iswitchb-initial-list))))

(defun laws-directory-files-recursive (parent match ext)
  (laws:fringe
   (mapcar (lambda (dir)
	     (directory-files dir nil ext))
	   (laws:filter 'file-directory-p (directory-files parent t match)))))

(defun laws-names-list ()
  "登録法令名と略称法令名のリストを返す。"
  (or laws-names-list
      (setq laws-names-list
	    (let ((result nil))
	      ;; 登録法令名
	      (do ((xs (laws-alist) (cdr xs)))
		  ((null xs))
		(do ((ys (cdar xs) (cdr ys)))
		    ((null ys))
		  (push (car (caar ys)) result)))
	      ;; 略称法令名
	      (do ((xs (laws-abbrev) (cdr xs)))
		  ((null xs) (delete-dups (nreverse result)))
		(do ((ys (cdar xs) (cdr ys)))
		    ((null ys) result)
		  (push (substring (caar ys) 1 -1) result)))))))

(defun laws-download-list (type)
  (when (file-exists-p laws-htmldata-path)
    (let ((func
	   (case type
	     (id (lambda (f)
		   (upcase (file-name-sans-extension f))))
	     (name (lambda (f)
		     (laws-get-name (upcase (file-name-sans-extension f))))))))
      (mapcar func
	      (laws-directory-files-recursive
	       laws-htmldata-path "\\`[MTSH][0-9]+\\'"
	       (concat "\\.html\\'"))))))

(defun laws-iswitchb-download-list () (laws-download-list 'name))

(defun laws-iswitchb-bookmark-list ()
  (mapcar (lambda (x) (laws-get-name x)) (laws-bookmark-alist)))


;;
;; laws-mode
;;
(easy-menu-define laws-mode-menu
    laws-mode-map
  "laws-mode-menu"
  '("Laws"
    ["Digit Argument 0..9 -" nil]
    ["Search And Push Anchor" laws-search-or-push-anchor t]
    ["Laws Iswitchb" laws-iswitchb t]
    ["Forward Anchor" laws-forward-anchor t]
    ["Backward Anchor" laws-backward-anchor t]
    "-"
    ["Browse This URL" laws-browse-current-url t]
    "-"
    ["Toggle Fontify Parens" laws-fontify-or-defontify-paren t]
    ["Compose Parens" laws-compose-paren t]
    ["Decompose Parens" laws-decompose-paren t]
    "-"
    ["Winconf Add" laws-winconf-add t]
    ["Winconf Insert" laws-winconf-insert t]
    ["Winconf Override" laws-winconf-override t]
    ["Winconf Delete" laws-winconf-delete t]
    ["Winconf Backward Delete" laws-winconf-backward-delete t]
    ["Winconf Delete All" laws-winconf-delete-all t]
    ["Winconf Message" laws-winconf-message t]
    "-"
    ["Restore Current Winconf" laws-restore-current-winconf t]
    ["Restore First Winconf" laws-restore-first-winconf t]
    ["Restore Last Winconf" laws-restore-last-winconf t]
    ["Forward Winconf" laws-forward-winconf t]
    ["Backward Winconf" laws-backward-winconf t]
    "-"
    ["Display Other buffer" laws-display-toggle t]
    ["Other Window" laws-other-window t]
    "-"
    ["Forward Parens" laws-forward-paren t]
    ["Forward Article" laws-forward-article t]
    ["Backward Article" laws-backward-article t]
    ["Forward Paragraph" laws-forward-paragraph t]
    ["Backward Paragraph" laws-backward-paragraph t]
    "-"
    ["Help" laws-help t]
    ["View Quit And Return Index" laws-view-quit t]
    ))

(defun laws-mode ()
  (interactive)
  (unless (buffer-file-name)
    (error "No visited file."))
  (unless (equal (laws-expand-data-file
		  (file-name-sans-extension
		   (file-name-nondirectory (buffer-file-name))))
		 (buffer-file-name))
    (message "File `%s' is irregular law's path name." (buffer-file-name))
    (sleep-for 3))
  (kill-all-local-variables)
  (use-local-map laws-mode-map)
  (setq mode-name laws-mode-name)
  (setq major-mode 'laws-mode)
  (setq buffer-read-only t)
  (set (make-local-variable 'laws-paren-overlays) nil)
  (set (make-local-variable 'font-lock-defaults)
       '(laws-font-lock-keywords))
  (let ((init (laws-read-init-file)))
    ;; font-lock-keywords
    (setq laws-font-lock-keywords
	  (append laws-font-lock-keywords-0
		  laws-font-lock-keywords-1
		  (let ((regs (car init)))
		    ;; `laws-font-lock-keywords-2'
		    ;; 法令ファイルごとにバッファローカルな設定。
		    ;; `laws-anchor-clickable'
		    (cond ((and regs laws-anchor-clickable)
			   (list `(,regs (0 laws-anchor-name-face t))
				 `(,regs ,(laws-set-mouse-face-2 0))))
			  (regs (list `(,regs (0 laws-anchor-name-face t))))
			  (t nil)))))
    ;; iimage
    (when (nth 2 init)
      (iimage-mode 1))
    ;; 未施行法令のリンク先設定
    (set (make-local-variable 'laws-mishikou-list)
	 (cadr (laws-read-init-file))))
  (turn-on-font-lock)
  (setq line-spacing laws-line-space)
  ;; imenu
  (setq imenu-generic-expression
	'((nil "^第[一二三四五六七八九十百千のノ]+条.\\{1,20\\}" 0)))
  (imenu-add-to-menubar "Articles")
  (easy-menu-add laws-mode-menu)
  ;; outline
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (laws-outline-regexps)
  (and laws-mode-line
       (setq mode-line-buffer-identification laws-mode-line))
  (run-hooks 'laws-mode-hook))

(defvar laws-setup-p t
  "Non-nil means do setup, else not setup.")

(defun laws-setup ()
  (when laws-setup-p

    (when laws-use-buffer-law-name
      (add-hook 'laws-mode-hook 'laws-rename-buffer))

    ;; Read bookmark file.
    (laws-bookmark-alist)

    ;; Read Recent file.
    (laws-recent-alist)

    ;; `laws-bookmark-alist'
    (add-hook 'kill-emacs-hook 'laws-bookmark-save)

    ;; `laws-recent-alist'
    (add-hook 'kill-emacs-hook 'laws-recent-save)

    ;; `laws-recent-add'
    (add-hook 'find-file-hook 'laws-recent-add)

    (setq laws-setup-p nil)
    (message "Laws setup...done")))

(defun laws-exit ()
  (interactive)
  (unless (memq major-mode '(laws-mode laws-index-mode))
    (error ""))
  (unless (y-or-n-p "Exit laws? ")
    (error ""))
  (laws-bookmark-save)
  (laws-recent-save)
  (remove-hook 'kill-emacs-hook 'laws-recent-save)
  (remove-hook 'kill-emacs-hook 'laws-bookmark-save)
  (mapc (lambda (cel)
	  (kill-buffer (get-file-buffer (laws-expand-data-file (cdr cel)))))
	(laws-opened-alist))
  (setq laws-alist nil
	laws-abbrev nil
	laws-abbrev-alist nil
	laws-bookmark-alist nil
	laws-recent-alist nil
	laws-opened-alist nil
	laws-winconf-list nil
	laws-winconf-index 0
	laws-display-toggle-winconf nil
	laws-names-list nil
	laws-iswitchb-present-list nil)
  (setq laws-setup-p t)
  (message "Initialize laws variables...done")
  (when (get-buffer laws-index-buffer)
    (kill-buffer laws-index-buffer))
  (message "Kill all laws buffers...done"))

(add-hook 'laws-index-mode-hook 'laws-setup)
(add-hook 'laws-mode-hook 'laws-setup)

(provide 'laws)

;;; laws.el ends here
