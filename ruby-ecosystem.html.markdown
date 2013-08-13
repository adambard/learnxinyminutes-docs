---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]

---

People using ruby generally have a way to install different ruby versions,
manage their packages (or gems), and manage their gem dependencies.

## Ruby Managers

Some platforms have ruby pre-installed or available as a package. Most rubyists
do not use these, or if they do, they only use them to bootstrap another ruby
installer or implementation. Instead rubyists tend to install a ruby manager to
install and switch between many versions of ruby and their projects' ruby
environments.

The following are the popular ruby/environment managers:

* [RVM](https://rvm.io/) - Installs and switches between rubies. RVM also has
  the concept of gemsets to isolate projects' environments completely.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Only installs
  rubies. Use this for finer control over your rubies' installations.
* [rbenv](https://github.com/sstephenson/rbenv) - Only switches between rubies.
  Used with ruby-build.  Use this for finer control over how rubies load.
* [chruby](https://github.com/postmodern/chruby) - Only switches between rubies.
  Similar in spirit to rbenv. Unopinionated about how rubies are installed.

## Ruby Versions

Ruby was created by Yukihiro "Matz" Matsumoto, who remains somewhat of a
[BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life), although
that is changing recently. As a result, the reference implementation of ruby is
called MRI (Matz' Reference Implementation), and when you hear a ruby version,
it is referring to the release version of MRI.

The three major version of ruby in use are:

* 2.0.0 - Released in February 2013. Most major libraries and frameworks support
  2.0.0.
* 1.9.3 - Released in October 2011. This is the version most rubyists use
  currently.
* 1.8.7 - Ruby 1.8.7 has been
  [retired](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

The change between 1.8.7 to 1.9.x is a much larger change than 1.9.3 to 2.0.0.
For instance, the 1.9 series introduced encodings and a bytecode VM.  There
are projects still on 1.8.7, but they are becoming a small minority, as most of
the community has moved to at least 1.9.2 or 1.9.3.

## Ruby Implementations

The ruby ecosystem enjoys many different implementations of ruby, each with
unique strengths and states of compatability. To be clear, the different
implementations are written in different languages, but *they are all ruby*.
Each implementation has special hooks and extra features, but they all run
normal ruby files well. For instance, JRuby is written in Java, but you do
not need to know Java to use it.

Very mature/compatible:

* MRI - Written in C, this is the reference implementation of ruby. By
  definition it is 100% compatible (with itself). All other rubies
maintain capatability with MRI (see RubySpec below).
* JRuby - Written in Java and ruby, this robust implementation is quite fast.
  Most importantly, JRuby's strength is JVM/Java interop, leveraging existing
JVM tools, projects, and languages.
* Rubinius - Written primarily in ruby itself with a C++ bytecode VM. Also
  mature and fast. Because it is implemented in ruby itself, it exposes many VM
features into rubyland.

Medium mature/compatible:

* Maglev - Built on top of Gemstone, a Smalltalk VM. Smalltalk has some
  impressive tooling, and this project tries to bring that into ruby
development.
* RubyMotion - Brings ruby to iOS development.

Less mature/compatible:

* Topaz - Written in RPython (using the PyPy toolchain), Topaz is fairly young
  and not yet compatable. It shows promise to be a high-performance ruby
implementation.
* IronRuby - Written in C# targeting the .NET platform, work on IronRuby seems
  to have stopped since Microsoft pulled their support.

Ruby implementations may have their own release version numbers, but they always
target a specific version of MRI for compatability. Many implementations have
the ability to enter different modes (for example, 1.8 or 1.9 mode) to specify
which MRI version to target.

## RubySpec

Most ruby implementations rely heavily on [RubySpec](http://rubyspec.org/). Ruby
has no official specification, so the community has written executable specs in
ruby to test their implementations' compatability with MRI.

## RubyGems

[RubyGems](http://rubygems.org/) is a community-run package manager for ruby.
RubyGems ships with ruby, so there is no need to download it separately.

Ruby packages are called "gems," and they can be hosted by the community at
RubyGems.org. Each gem contains its source code and some metadata, including
things like version, dependencies, author(s), and license(s).

## Bundler

[Bundler](http://bundler.io/) is a gem dependency resolver. It uses a project's
Gemfile to find dependencies, and then fetches those dependencies' dependencies
recursively. It does this until all dependencies are resolved and downloaded, or
it will stop if a conflict has been found.

Bundler will raise an error if it finds conflicting dependencies. For example,
if gem A requires version 3 or greater of gem Z, but gem B requires version 2,
Bundler will notify you of the conflict. This becomes extremely helpful as many
gems refer to other gems (which refer to other gems), which can form a large
dependency graph to resolve.

# Testing

Testing is a large of ruby culture. Ruby comes with its own Unit-style testing
framework called minitest (Or TestUnit for ruby version 1.8.x). There are many
testing libraries with different goals.

* TestUnit - Ruby 1.8's built-in "Unit-style" testing framework
* minitest - Ruby 1.9/2.0's built-in testing framework
* RSpec - A testing framework that focuses on expressivity
* Cucumber - A BDD testing framework that parses Gherkin formatted tests

## Be Nice

The ruby community takes pride in being an open, diverse, welcoming community.
Matz himself is extremely friendly, and the generosity of rubyists on the whole
is amazing.
