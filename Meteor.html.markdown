---
language: Meteor.js
Filename: Meteor.html.markdown 
contributors:
    - ["Mohammed Rafy", "https://github.com/IamRafy/"]
   
---



Meteor is an ultra-simple environment for building modern websites. What once took weeks, even with the best tools, now takes hours with Meteor.

The web was originally designed to work in the same way that mainframes worked in the 70s. The application server rendered a screen and sent it over the network to a dumb terminal. Whenever the user did anything, that server rerendered a whole new screen. This model served the Web well for over a decade. It gave rise to LAMP, Rails, Django, PHP.

But the best teams, with the biggest budgets and the longest schedules, now build applications in JavaScript that run on the client. These apps have stellar interfaces. They don't reload pages. They are reactive: changes from any client immediately appear on everyone's screen.

They've built them the hard way. Meteor makes it an order of magnitude simpler, and a lot more fun. You can build a complete application in a weekend, or a sufficiently caffeinated hackathon. No longer do you need to provision server resources, or deploy API endpoints in the cloud, or manage a database, or wrangle an ORM layer, or swap back and forth between JavaScript and Ruby, or broadcast data invalidations to clients.
Meteor Supports  OS X, Windows, and Linux. // https://github.com/meteor/meteor/wiki/Supported-Platforms
On Windows? https://install.meteor.com/windows
On OS X or Linux? Install the latest official Meteor release from your terminal:
$ curl https://install.meteor.com/ | sh
The Windows installer supports Windows 7, Windows 8.1, Windows Server 2008, and Windows Server 2012. The command line installer supports Mac OS X 10.7 (Lion) and above, and Linux on x86 and x86_64 architectures.

Once you've installed Meteor, create a project:
meteor create myapp
Run it locally:

cd myapp
meteor
# Meteor server running on: http://localhost:3000/

Then, open a new terminal tab and unleash it on the world (on a free server we provide):

meteor deploy myapp.meteor.com

Principles of Meteor

* Data on the Wire. Meteor doesn't send HTML over the network. The server sends data and lets the client render it.

* One Language. Meteor lets you write both the client and the server parts of your application in JavaScript.

* Database Everywhere. You can use the same methods to access your database from the client or the server.

* Latency Compensation. On the client, Meteor prefetches data and simulates models to make it look like server method calls return instantly.

* Full Stack Reactivity. In Meteor, realtime is the default. All layers, from database to template, update themselves automatically when necessary.

* Embrace the Ecosystem. Meteor is open source and integrates with existing open source tools and frameworks.

* Simplicity Equals Productivity. The best way to make something seem simple is to have it actually be simple. Meteor's main functionality has clean, classically beautiful APIs.

Developer Resources
-------------------

If anything in Meteor catches your interest, we hope you'll get involved with the project!

TUTORIAL
Get started fast with the official Meteor tutorial! https://www.meteor.com/install

STACK OVERFLOW
The best place to ask (and answer!) technical questions is on Stack Overflow. Be sure to add the meteor tag to your question. 
http://stackoverflow.com/questions/tagged/meteor

FORUMS
Visit the Meteor discussion forumsto announce projects, get help, talk about the community, or discuss changes to core. 
https://forums.meteor.com/
 
GITHUB
The core code is on GitHub. If you're able to write code or file issues, we'd love to have your help. Please read Contributing to Meteor for how to get started. https://github.com/meteor/meteor

THE METEOR MANUAL
In-depth articles about the core components of Meteor can be found on the Meteor Manual. The first article is about Tracker, our transparent reactivity framework. More articles (covering topics like Blaze, Unibuild, and DDP) are coming soon! http://manual.meteor.com/

What is Meteor?
---------------

Meteor is two things:

A library of packages: pre-written, self-contained modules that you might need in your app.

There are about a dozen core Meteor packages that most any app will use. Two examples: webapp, which handles incoming HTTP connections, and templating, which lets you make HTML templates that automatically update live as data changes. Then there are optional packages like email, which lets your app send emails, or the Meteor Accounts series (accounts-password, accounts-facebook, accounts-ui, and others) which provide a full-featured user account system that you can drop right into your app. In addition to these "core" packages, there are thousands of community-written packages in Atmosphere, one of which might do just what you need.

A command-line tool called meteor.

meteor is a build tool analogous to make, rake, or the non-visual parts of Visual Studio. It gathers up all of the source files and assets in your application, carries out any necessary build steps (such as compiling CoffeeScript, minifying CSS, building npm modules, or generating source maps), fetches the packages used by your app, and outputs a standalone, ready-to-run application bundle. In development mode it can do all of this interactively, so that whenever you change a file you immediately see the changes in your browser. It's super easy to use out of the box, but it's also extensible: you can add support for new languages and compilers by adding build plugin packages to your app.

The key idea in the Meteor package system is that everything should work identically in the browser and on the server (wherever it makes sense, of course: browsers can't send email and servers can't capture mouse events). Our whole ecosystem has been built from the ground up to support this.

Structuring your application
----------------------------

A Meteor application is a mix of client-side JavaScript that runs inside a web browser or PhoneGap mobile app, server-side JavaScript that runs on the Meteor server inside a Node.js container, and all the supporting HTML templates, CSS rules, and static assets. Meteor automates the packaging and transmission of these different components, and it is quite flexible about how you choose to structure those components in your file tree.

Special Directories
-------------------

By default, any JavaScript files in your Meteor folder are bundled and sent to the client and the server. However, the names of the files and directories inside your project can affect their load order, where they are loaded, and some other characteristics. Here is a list of file and directory names that are treated specially by Meteor:

client

Any directory named client is not loaded on the server. Similar to wrapping your code in if (Meteor.isClient) { ... }. All files loaded on the client are automatically concatenated and minified when in production mode. In development mode, JavaScript and CSS files are not minified, to make debugging easier. (CSS files are still combined into a single file for consistency between production and development, because changing the CSS file's URL affects how URLs in it are processed.)

HTML files in a Meteor application are treated quite a bit differently from a server-side framework. Meteor scans all the HTML files in your directory for three top-level elements: <head>, <body>, and <template>. The head and body sections are separately concatenated into a single head and body, which are transmitted to the client on initial page load.

server
Any directory named server is not loaded on the client. Similar to wrapping your code in if (Meteor.isServer) { ... }, except the client never even receives the code. Any sensitive code that you don't want served to the client, such as code containing passwords or authentication mechanisms, should be kept in the server directory.

Meteor gathers all your JavaScript files, excluding anything under the client, public, and private subdirectories, and loads them into a Node.js server instance. In Meteor, your server code runs in a single thread per request, not in the asynchronous callback style typical of Node. We find the linear execution model a better fit for the typical server code in a Meteor application.

public

All files inside a top-level directory called public are served as-is to the client. When referencing these assets, do not include public/ in the URL, write the URL as if they were all in the top level. For example, reference public/bg.png as <img src='/bg.png' />. This is the best place for favicon.ico, robots.txt, and similar files.

private

All files inside a top-level directory called private are only accessible from server code and can be loaded via the Assets API. This can be used for private data files and any files that are in your project directory that you don't want to be accessible from the outside.

client/compatibility

This folder is for compatibility JavaScript libraries that rely on variables declared with var at the top level being exported as globals. Files in this directory are executed without being wrapped in a new variable scope. These files are executed before other client-side JavaScript files.
tests

Any directory named tests is not loaded anywhere. Use this for any local test code.

node_modules

For compatibility with node.js tools used alongside Meteor, any directory named node_modules is not loaded anywhere. node.js packages installed into node_modules directories will not be available to your Meteor code. Use Npm.depends in your package package.js file for that.

Files outside special directories
---------------------------------

All JavaScript files outside special directories are loaded on both the client and the server. That's the place for model definitions and other functions. Meteor provides the variables Meteor.isClient and Meteor.isServer (http://docs.meteor.com/#/full/meteor_isserver)so that your code can alter its behavior depending on whether it's running on the client or the server.

CSS and HTML files outside special directories are loaded on the client only, and cannot be used from server code.

Example File Structure

The file structure of your Meteor app is very flexible. Here is an example layout that takes advantage of some of the special folders mentioned above.

lib/                      # common code like collections and utilities
lib/methods.js            # Meteor.methods definitions
lib/constants.js          # constants used in the rest of the code

client/compatibility      # legacy libraries that expect to be global
client/lib/               # code for the client to be loaded first
client/lib/helpers.js     # useful helpers for your client code
client/body.html          # content that goes in the <body> of your HTML
client/head.html          # content for <head> of your HTML: <meta> tags, etc
client/style.css          # some CSS code
client/<feature>.html     # HTML templates related to a certain feature
client/<feature>.js       # JavaScript code related to a certain feature

server/lib/permissions.js # sensitive permissions code used by your server
server/publications.js    # Meteor.publish definitions

public/favicon.ico        # app icon
settings.json             # configuration data to be passed to meteor --settings
mobile-config.js          # define icons and metadata for Android/iOS

You can also model your directory structure after the example apps. Run meteor create --example todos and explore the directories to see where all the files in a real app could go.

File Load Order
---------------

It is best to write your application in such a way that it is insensitive to the order in which files are loaded, for example by using Meteor.startup (http://docs.meteor.com/#/full/meteor_startup) , or by moving load order sensitive code into packages (http://docs.meteor.com/#/full/usingpackages) , which can explicitly control both the load order of their contents and their load order with respect to other packages. However, sometimes load order dependencies in your application are unavoidable.

There are several load ordering rules. They are applied sequentially to all applicable files in the application, in the priority given below:

HTML template files are always loaded before everything else
Files beginning with main. are loaded last
Files inside any lib/ directory are loaded next
Files with deeper paths are loaded next
Files are then loaded in alphabetical order of the entire path
nav.html
main.html
client/lib/methods.js
client/lib/styles.js
lib/feature/styles.js
lib/collections.js
client/feature-y.js
feature-x.js
client/main.js
For example, the files above are arranged in the correct load order. main.html is loaded second because HTML templates are always loaded first, even if it begins with main., since rule 1 has priority over rule 2. However, it will be loaded after nav.html because rule 2 has priority over rule 5.

client/lib/styles.js and lib/feature/styles.js have identical load order up to rule 4; however, since client comes before lib alphabetically, it will be loaded first.

Organizing Your Project
-----------------------

There are three main ways to organize your files into features or components. Let's say we have two types of objects in our project: apples and oranges.

Method 1: Root-Level Folders
Since the special client, server, and lib directories work if they are anywhere in the path, you can use top-level folders to organize code into modules:

apples/lib/               # code for apple-related features
apples/client/
apples/server/

oranges/lib/              # code for orange-related features
oranges/client/
oranges/server/

Method 2: Folders inside client/ and server/

lib/apples/               # common code for apples
lib/oranges/              # and oranges

client/apples/            # client code for apples
client/oranges/           # and oranges

server/apples/            # server code for apples
server/oranges/           # and oranges

Method 3: Packages

This is the ultimate in code separation, modularity, and reusability. If you put the code for each feature in a separate package, the code for one feature won't be able to access the code for the other feature except through exports, making every dependency explicit. This also allows for the easiest independent testing of features. You can also publish the packages and use them in multiple apps with meteor add.

packages/apples/package.js     # files, dependencies, exports for apple feature
packages/apples/<anything>.js  # file loading is controlled by package.js

packages/oranges/package.js    # files, dependencies, exports for orange feature
packages/oranges/<anything>.js # file loading is controlled by package.js

Data and security
-----------------

Meteor makes writing distributed client code as simple as talking to a local database. It's a clean, simple, and secure approach that removes the need to implement individual RPC endpoints, manually cache data on the client to avoid slow roundtrips to the server, and carefully orchestrate invalidation messages to every client as data changes.

In Meteor, the client and server share the same database API. The same exact application code — like validators and computed properties — can often run in both places. But while code running on the server has direct access to the database, code running on the client does not. This distinction is the basis for Meteor's data security model.

By default, a new Meteor app includes the autopublish and insecure packages, which together mimic the effect of each client having full read/write access to the server's database. These are useful prototyping tools, but typically not appropriate for production applications. When you're ready, just remove the packages.

Every Meteor client includes an in-memory database cache. To manage the client cache, the server publishes sets of JSON documents, and the client subscribes to those sets. As documents in a set change, the server patches each client's cache.

Today most Meteor apps use MongoDB as their database because it is the best supported, though support for other databases is coming in the future. The Mongo.Collection class is used to declare Mongo collections and to manipulate them. Thanks to minimongo, Meteor's client-side Mongo emulator, Mongo.Collection can be used from both client and server code.

// declare collections
// this code should be included in both the client and the server
Rooms = new Mongo.Collection("rooms");
Messages = new Mongo.Collection("messages");
Parties = new Mongo.Collection("parties");

// server: populate collections with some initial documents
Rooms.insert({name: "Conference Room A"});
var myRooms = Rooms.find({}).fetch();
Messages.insert({text: "Hello world", room: myRooms[0]._id});
Parties.insert({name: "Super Bowl Party"});
Each document set is defined by a publish function on the server. The publish function runs each time a new client subscribes to a document set. The data in a document set can come from anywhere, but the common case is to publish a database query.

// server: publish all room documents
Meteor.publish("all-rooms", function () {
  return Rooms.find(); // everything
});

// server: publish all messages for a given room
Meteor.publish("messages", function (roomId) {
  check(roomId, String);
  return Messages.find({room: roomId});
});

// server: publish the set of parties the logged-in user can see.
Meteor.publish("parties", function () {
  return Parties.find({$or: [{"public": true},
                             {invited: this.userId},
                             {owner: this.userId}]});
});
Publish functions can provide different results to each client. In the last example, a logged in user can only see Party documents that are public, that the user owns, or that the user has been invited to.

Once subscribed, the client uses its cache as a fast local database, dramatically simplifying client code. Reads never require a costly round trip to the server. And they're limited to the contents of the cache: a query for every document in a collection on a client will only return documents the server is publishing to that client.

// client: start a parties subscription
Meteor.subscribe("parties");

// client: return array of Parties this client can read
return Parties.find().fetch(); // synchronous!
Sophisticated clients can turn subscriptions on and off to control how much data is kept in the cache and manage network traffic. When a subscription is turned off, all its documents are removed from the cache unless the same document is also provided by another active subscription.

When the client changes one or more documents, it sends a message to the server requesting the change. The server checks the proposed change against a set of allow/deny rules you write as JavaScript functions. The server only accepts the change if all the rules pass.

// server: don't allow client to insert a party
Parties.allow({
  insert: function (userId, party) {
    return false;
  }
});

// client: this will fail
var party = { ... };
Parties.insert(party);
If the server accepts the change, it applies the change to the database and automatically propagates the change to other clients subscribed to the affected documents. If not, the update fails, the server's database remains untouched, and no other client sees the update.

Meteor has a cute trick, though. When a client issues a write to the server, it also updates its local cache immediately, without waiting for the server's response. This means the screen will redraw right away. If the server accepted the update — what ought to happen most of the time in a properly behaving client — then the client got a jump on the change and didn't have to wait for the round trip to update its own screen. If the server rejects the change, Meteor patches up the client's cache with the server's result.

Putting it all together, these techniques accomplish latency compensation. Clients hold a fresh copy of the data they need, and never need to wait for a roundtrip to the server. And when clients modify data, those modifications can run locally without waiting for the confirmation from the server, while still giving the server final say over the requested change.

The current release of Meteor supports MongoDB, the popular document database, and the examples in this section use the MongoDB API. Future releases will include support for other databases.

Authentication and user accounts
--------------------------------

Meteor includes Meteor Accounts, a state-of-the-art authentication system. It features secure password login using the bcrypt algorithm, and integration with external services including Facebook, GitHub, Google, Meetup, Twitter, and Weibo. Meteor Accounts defines a Meteor.users collection where developers can store application-specific user data.

Meteor also includes pre-built forms for common tasks like login, signup, password change, and password reset emails. You can add Accounts UI to your app with just one line of code. The accounts-ui package even provides a configuration wizard that walks you through the steps to set up the external login services you're using in your app.

Input validation
----------------

Meteor allows your methods and publish functions to take arguments of any JSON type. (In fact, Meteor's wire protocol supports EJSON, an extension of JSON which also supports other common types like dates and binary buffers.) JavaScript's dynamic typing means you don't need to declare precise types of every variable in your app, but it's usually helpful to ensure that the arguments that clients are passing to your methods and publish functions are of the type that you expect.

Meteor provides a lightweight library for checking that arguments and other values are the type you expect them to be. Simply start your functions with statements like check(username, String) or check(office, {building: String, room: Number}). The check call will throw an error if its argument is of an unexpected type.

Meteor also provides an easy way to make sure that all of your methods and publish functions validate all of their arguments. Just run meteor add audit-argument-checks and any method or publish function which skips checking any of its arguments will fail with an exception.

Reactivity
----------

Meteor embraces the concept of reactive programming (https://en.wikipedia.org/wiki/Reactive_programming). This means that you can write your code in a simple imperative style, and the result will be automatically recalculated whenever data changes that your code depends on.

Tracker.autorun(function () {
  Meteor.subscribe("messages", Session.get("currentRoomId"));
});
This example (taken from a chat room client) sets up a data subscription based on the session variable currentRoomId. If the value of Session.get("currentRoomId") changes for any reason, the function will be automatically re-run, setting up a new subscription that replaces the old one.

This automatic recomputation is achieved by a cooperation between Session and Tracker.autorun.  Tracker.autorun performs an arbitrary "reactive computation" inside of which data dependencies are tracked, and it will re-run its function argument as necessary. Data providers like Session, on the other hand, make note of the computation they are called from and what data was requested, and they are prepared to send an invalidation signal to the computation when the data changes.

This simple pattern (reactive computation + reactive data source) has wide applicability. Above, the programmer is saved from writing unsubscribe/resubscribe calls and making sure they are called at the right time. In general, Meteor can eliminate whole classes of data propagation code which would otherwise clog up your application with error-prone logic.

These Meteor functions run your code as a reactive computation:

Templates
Tracker.autorun
Template.autorun
Blaze.render and Blaze.renderWithData
And the reactive data sources that can trigger changes are:

Session variables
Database queries on Collections
Meteor.status
The ready() method on a subscription handle
Meteor.user
Meteor.userId
Meteor.loggingIn
In addition, the following functions which return an object with a stop method, if called from a reactive computation, are stopped when the computation is rerun or stopped:

Tracker.autorun (nested)
Meteor.subscribe
observe() and observeChanges() on cursors
Meteor's implementation is a package called Tracker that is fairly short and straightforward. You can use it yourself to implement new reactive data sources.

Live HTML templates
-------------------

HTML templating is central to web applications. With Blaze, Meteor's live page update technology, you can render your HTML reactively, meaning that it will update automatically to track changes in the data used to generate it.

Meteor makes it easy to use your favorite HTML templating language along with Meteor's live page update technology. Just write your template as you normally would, and Meteor will take care of making it update in realtime.

Meteor ships with a templating language called Spacebars (https://github.com/meteor/meteor/blob/master/packages/spacebars/README.md), inspired by Handlebars (http://handlebarsjs.com/). It shares some of the spirit and syntax of Handlebars, but it has been tailored to produce reactive Meteor templates when compiled.

Today, the only templating system that ships with Meteor is Spacebars, though our community has created packages for other languages such as Jade.

To define templates, create a file in your project with the .html extension. In the file, make a <template> tag and give it a name attribute. Put the template contents inside the tag. Meteor will precompile the template, ship it down to the client, and make it available as on the global Template object.

When your app is loaded, it automatically renders the special template called <body>, which is written using the <body> element instead of a <template>. You insert a template inside another template by using the {{> inclusion}} operator.

The easiest way to get data into templates is by defining helper functions in JavaScript. Define helpers with the Template.templateName.helpers({ ... }) function. Putting it all together:

<!-- in myapp.html -->
<body>
  <h1>Today's weather!</h1>
  {{> forecast}}
</body>

<template name="forecast">
  <div>It'll be {{prediction}} tonight</div>
</template>
// in client/myapp.js: reactive helper function
Template.forecast.helpers({
  prediction: function () {
    return Session.get("weather");
  }
});
// in the JavaScript console
> Session.set("weather", "cloudy");
> document.body.innerHTML
 => "<h1>Today's weather!</h1> <div>It'll be cloudy tonight</div>"

> Session.set("weather", "cool and dry");
> document.body.innerHTML
 => "<h1>Today's weather!</h1> <div>It'll be cool and dry tonight</div>"
To iterate over an array or database cursor, use {{#each}}:

<!-- in myapp.html -->
<template name="players">
  {{#each topScorers}}
    <div>{{name}}</div>
  {{/each}}
</template>
// in myapp.js
Template.players.helpers({
  topScorers: function () {
    return Users.find({score: {$gt: 100}}, {sort: {score: -1}});
  }
});
In this case, the data is coming from a database query. When the database cursor is passed to {{#each}}, it will wire up all of the machinery to efficiently add and move DOM nodes as new results enter the query.

Helpers can take arguments, and they receive the current template context data in this. Note that some block helpers change the current context (notably {{#each}} and {{#with}}):

// in a JavaScript file
Template.players.helpers({
  leagueIs: function (league) {
    return this.league === league;
  }
});
<!-- in a HTML file -->
<template name="players">
  {{#each topScorers}}
    {{#if leagueIs "junior"}}
      <div>Junior: {{name}}</div>
    {{/if}}
    {{#if leagueIs "senior"}}
      <div>Senior: {{name}}</div>
    {{/if}}
  {{/each}}
</template>
Helpers can also be used to pass in constant data.

// Works fine with {{#each sections}}
Template.report.helpers({
  sections: ["Situation", "Complication", "Resolution"]
});
Finally, you can use the events function on a template to attach event handlers. The object passed into events is documented at Event Maps. The this argument to the event handler will be the data context of the element that triggered the event.

<!-- myapp.html -->
<template name="scores">
  {{#each player}}
    {{> playerScore}}
  {{/each}}
</template>

<template name="playerScore">
  <div>{{name}}: {{score}}
    <span class="give-points">Give points</span>
  </div>
</template>
// myapp.js
Template.playerScore.events({
  'click .give-points': function () {
    Users.update(this._id, {$inc: {score: 2}});
  }
});
For more details about Spacebars, read the Spacebars README. (https://github.com/meteor/meteor/blob/master/packages/spacebars/README.md)

Using packages
--------------

All of the functionality you've read about so far is implemented in standard Meteor packages. This is possible thanks to Meteor's unusually powerful isomorphic package and build system. Isomorphic means the same packages work in the web browser, in mobile apps, and on the server. Packages can also contain plugins that extend the build process, such as coffeescript (CoffeeScript compilation : http://coffeescript.org/) or templating (compiling HTML templates).

Anyone can publish a Meteor package, and thousands of community-written packages have been published to date. The easiest way to browse these packages is Atmosphere, by Percolate Studio. You can also use the meteor search and meteor show commands.

You can add packages to your project with meteor add and remove them with meteor remove. Additionally, meteor list will tell you what packages your project is using, and meteor update will update them to the newest versions when possible.

By default all apps include the meteor-base package. This pulls in the packages that make up the core of the Meteor stack. Most apps will have this package.

All new apps also start with a set of packages that allow a friendly development experience. For more information about these packages, check out the comments in the packages file.

Meteor uses a single-loading packaging system, meaning that it loads just one version of every package. Before adding or upgrading to a particular version of a package, Meteor uses a constraint solver to check if doing so will cause other packages to break. By default, Meteor will choose conservatively. When adding transitive dependencies (packages that other packages, but not the application itself) depend on, Meteor will try to choose the earlier version.

In addition to the packages in the official Meteor release being used by your app, meteor list and meteor add also search the packages directory at the top of your app. You can also use the packages directory to break your app into subpackages for your convenience, or to test packages that you might want to publish. See Writing Packages (http://docs.meteor.com/#/full/writingpackages) . If you wish to add packages outside of your app's folder structure, set the environment variable PACKAGE_DIRS to a colon-delimited list of paths.

Namespacing
-----------

Meteor's namespacing support makes it easy to write large applications in JavaScript. Each package that you use in your app exists in its own separate namespace, meaning that it sees only its own global variables and any variables provided by the packages that it specifically uses. Here's how it works.

When you declare a top-level variable, you have a choice. You can make the variable File Scope or Package Scope.

// File Scope. This variable will be visible only inside this
// one file. Other files in this app or package won't see it.
var alicePerson = {name: "alice"};

// Package Scope. This variable is visible to every file inside
// of this package or app. The difference is that 'var' is
// omitted.
bobPerson = {name: "bob"};
Notice that this is just the normal JavaScript syntax for declaring a variable that is local or global. Meteor scans your source code for global variable assignments and generates a wrapper that makes sure that your globals don't escape their appropriate namespace.

In addition to File Scope and Package Scope, there are also Exports. An export is a variable that a package makes available to you when you use it. For example, the email package exports the Email variable. If your app uses the email package (and only if it uses the email package!) then your app can see Email and you can call Email.send. Most packages have only one export, but some packages might have two or three (for example, a package that provides several classes that work together).

You see only the exports of the packages that you use directly. If you use package A, and package A uses package B, then you only see package A's exports. Package B's exports don't "leak" into your namespace just because you used package A. This keeps each namespace nice and tidy. Each app or package only sees their own globals plus the APIs of the packages that they specifically asked for.

When debugging your app, your browser's JavaScript console behaves as if it were attached to your app's namespace. You see your app's globals and the exports of the packages that your app uses directly. You don't see the variables from inside those packages, and you don't see the exports of your transitive dependencies (packages that aren't used directly by your app, but that are used by packages that are used by your app).

If you want to look inside packages from inside your in-browser debugger, you've got two options:

Set a breakpoint inside package code. While stopped on that breakpoint, the console will be in the package's namespace. You'll see the package's package-scope variables, imports, and also any file-scope variables for the file you're stopped in.

If a package foo is included in your app, regardless of whether your app uses it directly, its exports are available in Package.foo. For example, if the email package is loaded, then you can access Package.email.Email.send even from namespaces that don't use the email package directly.

When declaring functions, keep in mind that function x () {} is just shorthand for var x = function x () {} in JavaScript. Consider these examples:

// This is the same as 'var x = function x () ...'. So x() is
// file-scope and can be called only from within this one file.
function x () { ... }

// No 'var', so x() is package-scope and can be called from
// any file inside this app or package.
x = function () { ... }
Technically speaking, globals in an app (as opposed to in a package) are actually true globals. They can't be captured in a scope that is private to the app code, because that would mean that they wouldn't be visible in the console during debugging! This means that app globals actually end up being visible in packages. That should never be a problem for properly written package code (since the app globals will still be properly shadowed by declarations in the packages). You certainly shouldn't depend on this quirk, and in the future Meteor may check for it and throw an error if you do.

Deploying
---------

Meteor is a full application server. We include everything you need to deploy your application on the internet: you just provide the JavaScript, HTML, and CSS.

Running on Meteor's infrastructure
----------------------------------

The easiest way to deploy your application is to use meteor
deploy. We provide it because it's what, personally, we've always wanted: an easy way to take an app idea, flesh it out over a weekend, and put it out there for the world to use, with nothing getting in the way of creativity.

meteor deploy myapp.meteor.com
Your application is now available at myapp.meteor.com. If this is the first time deploying to this hostname, Meteor creates a fresh empty database for your application. If you want to deploy an update, Meteor will preserve the existing data and just refresh the code.

You can also deploy to your own domain. Just set up the hostname you want to use as a CNAME to origin.meteor.com, then deploy to that name.

meteor deploy www.myapp.com
We provide this as a free service so you can try Meteor. It is also helpful for quickly putting up internal betas, demos, and so on. For more information, see meteor deploy.

Running on your own infrastructure
----------------------------------

You can also run your application on your own infrastructure or any hosting provider that can run Node.js apps.

To get started, run

meteor build my_directory
This command will generate a fully-contained Node.js application in the form of a tarball. To run this application, you need to provide Node.js 0.10 and a MongoDB server. (The current release of Meteor has been tested with Node 0.10.40.) You can then run the application by invoking node, specifying the HTTP port for the application to listen on, and the MongoDB endpoint.

cd my_directory
(cd programs/server && npm install)
env PORT=3000 MONGO_URL=mongodb://localhost:27017/myapp node main.js
Some packages might require other environment variables. For example, the email package requires a MAIL_URL environment variable.

Writing packages
----------------

Writing Meteor packages is easy. To initialize a meteor package, run meteor create --package username:packagename, where username is your Meteor Developer username. This will create a package from scratch and prefill the directory with a package.js control file and some javascript. By default, Meteor will take the package name from the name of the directory that contains the package.js file. Don't forget to run meteor add [packagename], even if the package is internal to the app, in order to use it.

Meteor promises repeatable builds for both packages and applications. This means that, if you built your package on a machine, then checked the code into a repository and checked it out elsewhere, you should get the same result. In your package directory, you will find an automatically generated .versions file. This file specifies the versions of all packages used to build your package and is part of the source. Check it into version control to ensure repeatable builds across machines.

Sometimes, packages do not just stand on their own, but function in the context of an app (specifically, packages in the packages directory of an app). In that case, the app's context will take precedence. Rather than using the .versions file as a guide, we will build the package with the same dependencies as used by the app (we think that, in practice, it would be confusing to find your local packages built with different versions of things).

Meteor uses extended semver versioning for its packages: that means that the version number has three parts separated by dots: major version, minor version and patch version (for example: 1.2.3) with an optional pre-release version. You can read more about it on semver.org. Additionally, because some meteor packages wrap external libraries, Meteor supports the convention of using _ to denote a wrap number.

You can read more about package.js (http://docs.meteor.com/#/full/packagejs) files in the API section.

A word on testing: since testing is an important part of the development process, there are two common ways to test a package:

Integration tests (putting a package directly into an application, and writing tests against the application) is the most common way to test a package. After creating your package, add it to your app's /packages directory and run meteor add. This will add your package to your app as a local package. You can then test and run your app as usual. Meteor will detect and respond to changes to your local package, just as it does to your app files.

Unit tests are run with the command meteor test-packages package-name. As described in the package.js section, you can use the package.js file to specify where your unit tests are located. If you have a repository that contains only the package source, you can test your package by specifying the path to the package directory (which must contain a slash), such as meteor test-packages ./.

To publish a package, run meteor publish from the package directory. There are some extra restrictions on published packages: they must contain a version (Meteor packages are versioned using strict semver versioning) and their names must be prefixed with the username of the author and a colon, like so: iron:router. This namespacing allows for more descriptive and on-topic package names.




