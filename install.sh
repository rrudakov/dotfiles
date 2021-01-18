#!/bin/bash

######################################################################
# install.sh
# This script creates symlinks in the home firectory for any files in
# ~/dotfiles directory
######################################################################

############### Variables:

dir=${HOME}/dotfiles
files="Xresources \
  emacs.d/early-init.el \
  emacs.d/init.el \
  emacs.d/emacs.org \
  emacs.d/custom/google-java-format.el \
  emacs.d/templates/default-java.el \
  tmux.conf \
  vimrc \
  xmonad/xmonad.hs \
  xmonad/build \
  xmobarrc \
  xinitrc \
  gitconfig \
  stalonetrayrc \
  bashrc \
  bash_profile \
  aliasrc \
  config/dunst/dunstrc \
  config/fontconfig/fonts.conf \
  config/rofi/config.rasi \
  config/alacritty/alacritty.yml"

###############

# Changing to the dotfiles directory

echo "Changing to the $dir directory"
cd ${dir}
echo "...done"

echo "Create directories"
mkdir -p ~/.local/bin
mkdir -p ${HOME}/.xmonad ${HOME}/.emacs.d/custom ${HOME}/.config/dunst ${HOME}/.config/fontconfig ${HOME}/.config/alacritty ${HOME}/.emacs.d/templates
echo "...done"

# Remove all files inside .xmonad dir
if [ $# -eq 0 ]; then
    rm -rf ${HOME}/.xmonad/*
else
    rm ${HOME}/.xmonad/build
    rm ${HOME}/.xmonad/xmonad.hs
fi

# Move any existing dotfiles to ~/dotfiles_old, then creating symlinks
for file in $files; do
    echo "Creating symlinks for $file in home directory"
    ln -sf ${dir}/${file} ${HOME}/.${file}
done
echo "...done"

# Install executable scripts
echo "Create symlinks for executables"
ln -sf ${dir}/switch_monitor.sh ${HOME}/.local/bin/switch_monitor
ln -sf ${dir}/update_mail_index.sh ${HOME}/.local/bin/update_mail_index
echo "...done"

# Stop here if --configs
if [ "$1" == "--configs" ]; then
    echo "Skip the rest..."
    exit 0
fi

# Install vim vundle
echo "Installing vim vundle plugin manager"
rm -rf ${HOME}/.vim/bundle/Vundle.vim
git clone https://github.com/VundleVim/Vundle.vim.git ${HOME}/.vim/bundle/Vundle.vim || exit 1
echo "...done"

# Install xmonad
# System dependencies:
# libxss-dev
# libasound2-dev
# libxrandr-dev
# libxft-dev
# libxpm-dev

echo "Installing xmonad"
cd ${HOME}/.xmonad
echo "Clone xmonad repo"
git clone https://github.com/xmonad/xmonad || exit 1
cd xmonad && git checkout tags/v0.15 && cd .. || exit 1
echo "...done"
echo "Clone xmonad-contrib repo"
git clone https://github.com/xmonad/xmonad-contrib || exit 1
cd xmonad-contrib && git checkout tags/v0.16 && cd .. || exit 1
echo "...done"
echo "Clone xmonad-extras repo"
git clone https://github.com/xmonad/xmonad-extras.git || exit 1
echo "...done"
echo "Clone xmobar repo"
git clone https://github.com/jaor/xmobar || exit 1
echo "...done"
echo "Creating stack project"
stack init --resolver lts-16.27
echo "...done"
echo "Replace flags for xmobar in stack.yaml"
/usr/bin/sed -i "/flags: {}/c\flags:" stack.yaml || exit 1
/usr/bin/sed -i "/flags:/a\  xmobar:" stack.yaml || exit 1
/usr/bin/sed -i "/xmobar:/a\    with_threaded: true" stack.yaml || exit 1
/usr/bin/sed -i "/with_threaded: true/a\    with_utf8: true" stack.yaml || exit 1
/usr/bin/sed -i "/with_utf8: true/a\    with_xpm: true" stack.yaml || exit 1
/usr/bin/sed -i "/with_xpm: true/a\    with_xft: true" stack.yaml || exit 1
/usr/bin/sed -i "/with_xft: true/a\  xmonad-extras:" stack.yaml || exit 1
/usr/bin/sed -i "/xmonad-extras:/a\    with_mpd: false" stack.yaml || exit 1
echo "...done"
echo "Build and install xmonad and dependencies"
stack install || exit 1
echo "...done"
