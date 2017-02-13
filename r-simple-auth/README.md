## About
This is a simple implementation of the auth interface. It provides a login and registration form, and handles authentication of users through a password. The password is saved hashed and salted. An administration panel is added (if the admin interface is implemented) to allow users to change their password.

## Configuration
Two configuration values are of relevance:

```
:salt         ; => The salt for the passwords. Is defaulted to a random string on first init.
:registration ; => If this is "open", then registrations for new accounts can be made.
```

Note that once you change the salt, all passwords will become invalid and will need to be reset.
