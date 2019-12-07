# Troubleshooting

This document goes through a couple options that may help you out.

### Firewalls

Some companies have a firewall.

These companies usually have set the `HTTP_PROXY` or `HTTPS_PROXY` environment variable on your computer. This is more common with Windows computers.

The result is that the request for `https://github.com/elm/compiler/releases/download/0.19.1/binary-for-windows-64-bit.gz` is being sent to a "proxy server" where they monitor traffic. Maybe they rule out certain domains, maybe they check data when it comes back from the actual URL, etc.

It is probably best to ask someone about the situation on this, but you can test things out by temporarily using an alternate `HTTPS_PROXY` value with something like this:

```
# Mac and Linux
HTTPS_PROXY=http://proxy.example.com npm install -g elm

# Windows
set HTTPS_PROXY=http://proxy.example.com
npm install -g elm
```

Check out [this document](https://www.npmjs.com/package/request#controlling-proxy-behaviour-using-environment-variables) for more information on how environment variables like `NO_PROXY`, `HTTP_PROXY`, and `HTTPS_PROXY` are handled by the npm.

<br/>

## Do you know what a `PATH` variable is?

When you run a command like `elm make src/Main.elm`, your computer starts by trying to find a file called `elm`.

The `PATH` is a list of directories to search within. On Mac and Linux, you can see these directories by running:

```
$ echo $PATH
/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin
```

The are separated by `:` for some reason. So running `elm make src/Main.elm` starts by searching the `PATH` for files named `elm`. On my computer, it finds `/usr/local/bin/elm` and then can actually run the command.

Is `elm` in one of the directories listed in your `PATH` variable? I recommend asking for help if you are in this scenario and unsure how to proceed.
