## About
This system provides an implementation for the `auth` and `user` interfaces by interacting with an LDAP server.

## Configuration
The following configuration properties are available:

    :ldap :host                   ; => The host of the LDAP server.
                                  ;    Defaults to localhost.
    :ldap :port                   ; => The port for the connection.
                                  ;    Defaults to 386.
    :ldap :ssl                    ; => Whether to use SSL with the connection.
                                  ;    Defaults to NIL.
    :ldap :base                   ; => The base DN where your user objects reside.
                                  ;    Typically "ou=People,dc=example,dc=com".
                                  ;    Defaults to NIL.
    :ldap :user                   ; => The user to authenticate to LDAP with.
                                  ;    Typically "cn=admin,dc=example,dc=com".
                                  ;    Defaults to NIL.
    :ldap :pass                   ; => The password for the connecting user account.
                                  ;    Defaults to NIL.
    :ldap :connections            ; => The number of connections to pool.
                                  ;    Defaults to 5.
    :account :object-class        ; => The structural class to use for new users.
                                  ;    Defaults to "inetOrgPerson".
    :account :registration        ; => Whether registration is allowed.
                                  ;    Defaults to NIL.
    :account :recovery :timeout   ; => The number of seconds a recovery link is active.
                                  ;    Defaults to (* 24 60 60)
    :account :activation :timeout ; => The number of seconds an account has to be activated.
                                  ;    Defaults to (* 24 60 60)
    :account :totp :digest        ; => The digest mechanism for 2-factor auth
                                  ;    Defaults to :SHA1
    :account :totp :digits        ; => The number of 2-factor auth digits to use
                                  ;    Defaults to 6
    :account :totp :period        ; => The 2-factor auth code validity period in seconds
                                  ;    Defaults to 30
    :account :totp :issuer        ; => An identifier for the 2-factor auth provider (you)
                                  ;    Defaults to "radiance"
Notes:

* Account recovery and activation are only available if the `mail` interface is implemented.
* On user creation the following attributes are automatically provided: `objectClass`, `cn`, `sn`, `accountID`, `accountName`, `accountPermission`.
* The connecting user requires in the very least modify permissions for `objectClass=radianceAccount` records in the configured LDAP base dn.
* In order to allow registration and in general creation of users, the connecting user obviously also requires the permission to create new records.

## LDAP Schema
This system requires and LDAP schema extension. If your server is set up with OLC (cn=config), then you can simply import the provided [`radiance.ldif`](radiance.ldif) file. Typically by running something like the following on the server:

    ldapadd -Q -Y EXTERNAL -H ldapi:/// -f radiance.ldif

Particulary it adds the attribute types `accountID`, `accountName`, `accountField`, `accountPermission`, and `accountRecovery`, and the object classes `radianceNextID`, and `radianceAccount` to the schema. These are necessary to manage account information.
