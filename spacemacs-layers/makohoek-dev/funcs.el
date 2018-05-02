;; add to list of directories to search for Info documentation files.
;; to add libc:
;; $ cd ~/info
;; $ wget https://www.gnu.org/software/libc/manual/info/libc-info.tar.gz
;; $ tar zxvf libc-info.tar.gz
;; $ install-info --info-dir=/home/julienm/info/ /home/julienm/info/libc.info
(add-to-list 'Info-default-directory-list "~/info")
