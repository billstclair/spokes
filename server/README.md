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

* `screen -S spokes-server`
* `npm run start:server`
