## About
This system provides an implementation for the `auth` and `user` interfaces by interacting with an LDAP server.

## Configuration
The following configuration properties are available:

    :ldap :host                 ; => The host of the LDAP server.
                                ;    Defaults to localhost.
    :ldap :port                 ; => The port for the connection.
                                ;    Defaults to 386.
    :ldap :ssl                  ; => Whether to use SSL with the connection.
                                ;    Defaults to NIL.
    :ldap :base                 ; => The base DN where your user objects reside.
                                ;    Typically "ou=People,dc=example,dc=com".
                                ;    Defaults to NIL.
    :ldap :user                 ; => The user to authenticate to LDAP with.
                                ;    Typically "cn=admin,dc=example,dc=com".
                                ;    Defaults to NIL.
    :ldap :pass                 ; => The password for the connecting user account.
                                ;     Defaults to NIL.
    :account :object-class      ; => The structural class to use for new users.
                                ;    Defaults to "inetOrgPerson".
    :account :registration      ; => Whether registration is allowed.
                                ;    Defaults to NIL.
    :account :recovery :subject ; => The email subject line for an account recovery request.
                                ;    Defaults to "Radiance Account Recovery".
    :account :recovery :message ; => The email message template for an account recovery request.
                                ;    Defaults to some relatively long message.

Notes:

* Account recovery is only available if the `mail` interface is implemented.
* The recovery message string is interpreted as a format control string and is passed two values, the name of the user, and the URL the user should follow to recover their account.
* On user creation the following attributes are automatically provided: `objectClass`, `cn`, `sn`, `accountID`, `accountName`, `accountPermission`.
* The connecting user requires in the very least modify permissions for `objectClass=radianceAccount` records in the configured LDAP base dn.
* In order to allow registration and in general creation of users, the connecting user obviously also requires the permission to create new records.

## LDAP Schema
This system requires and LDAP schema extension. If your server is set up with OLC (cn=config), then you can simply import the provided [`radiance.ldif`](radiance.ldif) file. Typically by running something like the following on the server:

    ldapadd -Q -Y EXTERNAL -H ldapi:/// -f radiance.ldif

Particulary it adds the attribute types `accountID`, `accountName`, `accountField`, `accountPermission`, and `accountRecovery`, and the object classes `radianceNextID`, and `radianceAccount` to the schema. These are necessary to manage account information.
