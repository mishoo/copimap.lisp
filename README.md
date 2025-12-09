# `copimap` - IMAP/Maildir library for Common Lisp
### _Mihai Bazon <mihai.bazon@gmail.com>_

An IMAP client library for Common Lisp, with some OfflineIMAP-like features
to synchronize your email (gmail too) into a Maildir, so you can then use
nice tools like Emacs and mu4e/notmuch.

Note: this is incomplete, but there's a lot of useful stuff in it, so I'm
publishing it as is. It works for me (I'm using it from the REPL). Seems
stable (been using it for a year now). A CLI interface would be nice, but
I'm not motivated to keep working on it — maybe someone else will.

## Synopsis

### Connect to a server and test some commands

Note that the base `imap` class doesn't do anything with the data it
receives, so we'll just enable some logging to see it (via Shinmera's
[verbose](https://shinmera.com/docs/verbose/) library).

    COPIMAP> (setf (v:repl-level) :debug)

    COPIMAP> (defparameter imap
               (make-instance 'imap
                              :host "SERVER-HOST-NAME-OR-IP"
                              :user "USERNAME"
                              :password "PASSWORD"
                              :use-ssl t))

    COPIMAP> (imap-connect imap)

    COPIMAP> ;; fetch UID, INTERNALDATE, ENVELOPE and FLAGS for message 1
    COPIMAP> (imap-command imap '(:fetch 1 (uid internaldate envelope flags)))

    COPIMAP> ;; same, but fetch messages 1..5
    COPIMAP> (imap-command imap '(:fetch (:range 1 5) (uid internaldate envelope flags)))

The `:password` can be a list (as in the next example), which is interpreted
as a script/command to run in order to get the password (at standard
output). If that's the case, the password itself is not stored in the object
instance; it's only used for authentication, then discarded.

For `:use-ssl` you can also specify `:starttls` if we should connect in
plain text and send STARTTLS before authentication, or if you'd really want
to auth in plain text, pass `:nope-just-send-my-password-in-clear-text`.

### Sync IMAP mailbox to a local Maildir folder

I'm not sure it's safe to try this on a huge mailbox, because it fetches
*all* new messages in a single IMAP request, so first time you connect it'll
fetch the whole of it (dozens or even hundreds of messages seems to work
just fine; I'm just not sure about tens of thousands). In my case, I already
had the Maildir imported from OfflineIMAP (see below).

    COPIMAP> (setf (v:repl-level) :debug)

    COPIMAP> (defparameter imap
               (make-instance 'imap+mailbox
                              :host "imap.gmail.com"
                              :user "user.name@gmail.com"
                              :password '("password-for-gmail.sh")
                              :use-ssl t
                              :mailbox-name "[Gmail]/All Mail"
                              :local-store "~/Maildir/Gmail/_Gmail_.All/"))

    COPIMAP> (imap-connect imap)

## What's in it

- a low-level class (`imap`) for connecting to an IMAP server, sending
  commands and receiving data (notifications).

- parser for IMAP events from server (converts to list structures).

- generator for IMAP commands from list structures.

- multi-threaded: once connection is authenticated, the `imap` class will
  setup a read thread which continuously listens for messages from the IMAP
  server and calls appropriate methods on your class (`imap-handle`).

  It *should* be safe to issue commands (`imap-command`) from another
  thread. A mutex will be locked while the command is being sent. There's
  also `imap-command-sync`, which will wait for the result.

- automatically reconnect when connection goes down for any reason (except
  when receiving a BYE from the server, then the read loop will stop; to
  reconnect in this case, one can define an appropriate method for
  `imap-handle`).

- support for `IDLE` command — the reader thread enters `IDLE` mode and
  server can send push notifications for new messages, changed flags, etc.

  It *should* be safe to send commands while idling (`DONE` is sent
  automatically, and `IDLE` mode will be resumed after your command
  finished).

- parser/writer for MIME headers.

- support for encoding/decoding mUTF-7 strings, which are commonly used for
  mailbox names (or labels, in GMail). This is activated only when IMAP4rev1
  is advertised. Otherwise strings are expected to be UTF-8 (for IMAP4rev2
  servers) but however, I found no server to test this with.

- support for local message storage in Maildir folders (class
  `imap+mailbox`).

- maintains information about the local store in a SQLite database, like
  OfflineIMAP.

- some local-to-remote synchronization support (flags, keywords, deletion).

## Installing

Clone the git repo and symlink the directory in
`~/quicklisp/local-projects/`. All dependencies are available in Quicklisp,
so then `(ql:quickload "copimap")` should work.

## Documentation

Start with the documentation in class `imap`. The other classes are not
documented, but in essence they just augment an IMAP connection with methods
for saving messages in Maildir and updating the database.

## Import DB from OfflineIMAP

Use function `maildir-import-offlineimap` (in [store.lisp](store.lisp)) for
each mailbox:

    COPIMAP> (maildir-import-offlineimap "~/Maildir/.../INBOX/")

It'll parse headers for each message, so it might take a while. It will
produce COPIMAP's SQLite database in the same folder, a file named
`.copimap.sqlite3`. After doing this you can pass the folder to
`:local-store`, as per the example in Synopsis, and when you connect it'll
only fetch new messages.

## Known quirks

- Tested on Linux only. Relies on the `find` POSIX utility when pushing
  local changes to remote (it's many times faster than calling `directory`
  and `stat` every file).

- Tested with SBCL only. It should work on other implementations, but there
  is SBCL-specific code for handling reconnect better. It also uses (if
  present) `TCP_USER_TIMEOUT` socket option, which I added to SBCL in a
  minor patch (https://github.com/sbcl/sbcl/pull/66).

  On other implementations, and depending on your system settings, it might
  take a long time for the reader thread to detect that connection is broken
  and attempt reconnecting. See also this:
  https://lisperator.net/blog/common-lisp-socket-client---reconnect-on-failure/

- Every now and then GMail sends "BYE (session expired)" and the read loop
  stops. I'm not bothered by that and I made no attempt to reconnect in such
  case, but the fix should be simple (see `imap-handle` method, there is an
  implementation for the `$BYE` notification which just closes the socket;
  we could reconnect there, but I'd rather try to understand why does GMail
  think my session expired...).

- Synchronization is incomplete. Fetching new messages works. However,
  message flags or labels which change on the remote will not be synced to
  the local Maildir (they do get saved in the SQLite database; but changing
  a message flags should alter the file name, and changing the labels on
  GMail should alter the message itself, to add or modify the `X-Keywords`
  header; this is not implemented).

  These work the other way around, though. If you view a message locally and
  its status gets changed from New to Seen, or if labels are added to its
  `X-Keywords` header, those changes can be pushed to remote by calling:

      (mailbox-push-local-changes imap)

  This is never called automatically. I thought I'd schedule a timer for
  that, but I didn't get to work on it yet.

  Locally deleted messages will also be deleted on remote when you call this
  function. (If remote is GMail, they'll be added the `\Trash` label and you
  can still find them there for 30 days).

- This README was written almost one year after the last meaningful commit.

## License

MIT
