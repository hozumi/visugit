# Visugit
Visugit is a visualization tool for git repository.<br>
<object id="scPlayer" class="embeddedObject" width="799" height="809" type="application/x-shockwave-flash" data="http://content.screencast.com/users/hozumi/folders/Jing/media/f38ec2ce-ac26-4d07-aff2-ea3ad109f26d/jingswfplayer.swf">
	<param name="movie" value="http://content.screencast.com/users/hozumi/folders/Jing/media/f38ec2ce-ac26-4d07-aff2-ea3ad109f26d/jingswfplayer.swf">
	<param name="quality" value="high">
	<param name="bgcolor" value="#FFFFFF">
	<param name="flashVars" value="containerwidth=799&amp;containerheight=809&amp;thumb=http://content.screencast.com/users/hozumi/folders/Jing/media/f38ec2ce-ac26-4d07-aff2-ea3ad109f26d/FirstFrame.jpg&amp;content=http://content.screencast.com/users/hozumi/folders/Jing/media/f38ec2ce-ac26-4d07-aff2-ea3ad109f26d/00000002.swf&amp;blurover=false">
	<param name="allowFullScreen" value="true">
	<param name="scale" value="showall">
	<param name="allowScriptAccess" value="always">
	<param name="base" value="http://content.screencast.com/users/hozumi/folders/Jing/media/f38ec2ce-ac26-4d07-aff2-ea3ad109f26d/">
</object>

## Install
Tested on OSX 10.6 and Ubuntu 10.04.<br>

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

Copyright (C) 2010 Takahiro Hozumi.

Distributed under the Eclipse Public License.