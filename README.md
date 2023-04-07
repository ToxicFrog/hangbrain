# hangbrain

A personal Google Chat to IRC bridge written in Clojure, using [zeiat](/toxicfrog/zeiat).

🛈 This project is not affiliated with or supported by Google. 🛈

⚠️ **This is not, in any sense, production-ready software.** ⚠️ It is a rapidly cobbled-together proof of concept that breaks frequently but works just well enough that I haven't yet needed to redo it properly. I don't recommend using it, but have published it as a learning experience.

## Functionality

Hangbrain acts as an IRC to Google Chat proxy -- you connect to it using an IRC client, and it connects to Chat on your behalf, using a headless browser, forwarding messages between the IRC client and the browser. It is **single user only**; you can't use it to host a general purpose IRC⟷Chat gateway or something. It's meant to be run on the same computer as your IRC client.

It supports a few command line flags for setting listen port, profile directory, and whatnot; run `lein run -- --help` for a list.

## Setup

- Make sure you have `firefox` and `geckodriver` installed.
- Choose a profile directory. The default is `~/.config/hangbrain/` and will be used for the rest of this README.
- Create it: `mkdir -p ~/.config/hangbrain/`.
- Start Firefox pointing at the Hangbrain profile: `firefox -profile ~/.config/hangbrain/`
- Browse to [Google Chat](https://chat.google.com), log in, and then close Firefox.

## Usage

- Start Hangbrain: `lein run -- --port 1234 --profile ~/.config/hangbrain --browser /usr/bin/firefox`
- Start your IRC client and connect to `localhost:1234` (or whatever port you ran it on)
- Use `/list` and `/who` to list channels and DMs, and `/join` and `/query` to open them.

## License

Copyright © 2021 Rebecca "ToxicFrog" Kelly

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
