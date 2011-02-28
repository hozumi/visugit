# Visugit
Visugit is a visualization tool for git repository.<br>
<img src="https://github.com/downloads/hozumi/visugit/visugit_screen1.png" alt="visugit screen shot" title="visugit screenshot" align="center" />
## Install
I checked on OSX 10.6 and Ubuntu 10.04.<br>

    % wget --no-check-certificate https://github.com/hozumi/visugit/raw/stable/bin/visugit
    % chmod +x visugit
    % ./visugit upgrade
    The script at ./visugit will be upgraded to the latest stable version.
    -n Do you want to continue [Y/n]? y
    Upgrading...

*option*<br>
Install **visugit** command wherever you want.<br>
    % mv ./visugit /usr/local/bin/
    
## Usage
    % ./visugit ./your-git-project-dir-path


## Upgrade
    % ./visugit upgrade
    Upgrading...

    % ./visugit version
    visugit version is 1.0.0-SNAPSHOT

## Uninstall
    % rm ./visugit
    % rm -r ~/.visugit/

## License

Copyright (C) 2010 FIXME

Distributed under the Eclipse Public License.