This directory is for uploading to the web server machine for the server-side of the multi-user Spokes game.

The server runs in `Node.js`.

Installing Node & Elm in Ubuntu (as root):

* `apt-get install npm`
* `apt-get install nodejs-legacy`
* `npm install -g elm`
* `npm install -g elm-websocket-server`

Installing the server code (user account, see [`package.json`](package.json)) for details):

One time:

* `cd .../server   # this directory`
* `npm install`

To build after a code change:

* `npm run build:server`

To start the server:

* `npm run start:server`

If your web server automatically upgrades to HTTPS, you'll need to proxy to get to the non-encrypted websocket server. Do this by installing Apache `mod_proxy_wstunnel`:

    $ sudo a2enmod proxy_wstunnel
    $ sudo service apache2 restart

Then add to either your Apache virtual host configuration or to an `.htaccess` file, the following:

    ProxyPass "/spokes-server/"  "ws://localhost:8080/"
    
`/spokes-server/` has to match the contents of `site/server.txt`, from which the client loads the server default.
