(defun convert-from-simple (file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (loop for user in (user:list)
          for id from 0
          for name = (username user)
          for perms = (perms user)
          for fields = (fields user)
          do (format out "dn: cn=~a,ou=People,dc=tymoon,dc=eu~%" name)
             (format out "objectClass: inetOrgPerson~%")
             (format out "objectClass: radianceAccount~%")
             (format out "cn: ~a~%" name)
             (format out "sn: ~a~%" name)
             (format out "mail: ~a~%" (gethash fields "email"))
             (format out "accountID: ~a~%" id)
             (format out "accountName: ~a~%" name)
             (loop for field being the hash-keys of fields
                   for value being the hash-values of fields
                   do (format out "accountField: ~a=~a~%" field value))
             (loop for perm in (remove-duplicates perms :test #'equalp)
                   do (format out "accountPermission: ~a~%"
                              (format NIL "~{~(~a~).~}" perm)))
             (format out "~%"))))
