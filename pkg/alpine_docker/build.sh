#!/bin/sh -e

# This script is wrote by Sergey Safarov <s.safarov@gmail.com>

BUILD_ROOT=/tmp/kazoo
FILELIST=/tmp/filelist
FILELIST_BINARY=/tmp/filelist_binary
TMP_TAR=/tmp/kazoo_min.tar.gz
IMG_TAR=kazoo_img.tar.gz

prepare_build() {
apk add --no-cache findutils abuild build-base git wget bash python2 gcc expat-dev libressl-dev \
                   erlang erlang-dev erlang-parsetools erlang-tools erlang-crypto erlang-syntax-tools \
                   erlang-ssl erlang-eunit erlang-hipe erlang-public-key erlang-asn1 erlang-inets erlang-xmerl \
                   erlang-observer erlang-sasl erlang-debugger erlang-runtime-tools erlang-snmp erlang-wx \
                   musl-utils ghostscript grep tiff-tools fontconfig imagemagick elixir coreutils

    adduser -D build && addgroup build abuild
    echo "%abuild ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/abuild
    su - build -c "git config --global user.name 'Your Full Name'"
    su - build -c "git config --global user.email 'your@email.address'"
    su - build -c "abuild-keygen -a -i"
}

build_and_install(){
    cd /usr/src/kazoo
    REPO_OWNER=$(git remote get-url origin 2> /dev/null | sed -e 's|^.*github.com/||' -e 's|^git@github.com:||' -e 's|/.*\.git||')
    GIT_TAG=$(git rev-parse HEAD 2> /dev/null)
    if [ ! -z "$REPO_OWNER" ]; then
        sed -i -e "s:github.com/2600hz:github.com/$REPO_OWNER:" /usr/src/kazoo/pkg/alpine/APKBUILD
    fi
    if [ ! -z "$GIT_TAG" ]; then
        sed -i -e "s/^_gitcommit=.*/_gitcommit=$GIT_TAG/" /usr/src/kazoo/pkg/alpine/APKBUILD
    fi
    chown -R build /usr/src/kazoo
    su - build -c "cd /usr/src/kazoo/pkg/alpine; abuild snapshot"
    su - build -c "cd /usr/src/kazoo/pkg/alpine; abuild -r"
    cd /home/build/packages/pkg/x86_64/
    ls -1 kazoo-*.apk |  xargs apk --no-cache --allow-untrusted add
    ln -s /opt/kazoo/bin/kazoo /usr/local/bin/kazoo
    ln -s /opt/kazoo/bin/nodetool /usr/local/bin/nodetool
    ln -s /opt/kazoo/bin/sup /usr/local/bin/sup
}

kazoo_files() {
    local PACKAGES
    PACKAGES=$(apk info | grep kazoo)
    PACKAGES="musl bash ghostscript fontconfig imagemagick $PACKAGES"
    for pkg in $PACKAGES
    do
        # list package files and filter package name
        apk info --contents $pkg 2> /dev/null | sed -e '/\S\+ contains:/d'  -e '/^$/d' -e 's/^/\//'
    done
}

extra_files() {
    cat << EOF
/etc
/bin
/bin/busybox
/usr/bin
/usr/bin/iconv
/usr/bin/ldd
/usr/bin/tiff2pdf
/usr/bin/tiffinfo
/usr/lib
/usr/local/bin/kazoo
/usr/local/bin/nodetool
/usr/local/bin/sup
/var
/var/run
/run
/tmp
EOF
}

sort_filelist() {
    sort $FILELIST | uniq > $FILELIST.new
    mv -f $FILELIST.new $FILELIST
}

filter_unnecessary_files() {
# excluded following files and directories recursive
# /usr/share/man
# /usr/share/snmp

    sed -i \
        -e '\|^/usr/share/man/|d' \
        -e '\|^/usr/share/snmp/|d' \
        $FILELIST
}

ldd_helper() {
    TESTFILE=$1
    LD_PRELOAD=/opt/kazoo/erts-9.3/bin/beam.smp ldd $TESTFILE 2> /dev/null > /dev/null || return

    LD_PRELOAD=/opt/kazoo/erts-9.3/bin/beam.smp ldd $TESTFILE | sed -e 's/^.* => //' -e 's/ (.*)//' -e 's/\s\+//' -e '/^ldd$/d'
}

find_binaries() {
    rm -f $FILELIST_BINARY
    set +e
    for f in $(cat $FILELIST)
    do
        ldd_helper /$f >> $FILELIST_BINARY
    done
    set -e
    sort $FILELIST_BINARY | sort | uniq > $FILELIST_BINARY.new
    mv -f $FILELIST_BINARY.new $FILELIST_BINARY

    # Resolving symbolic links and removing duplicates
    cat $FILELIST_BINARY | xargs realpath > $FILELIST_BINARY.new
    cat $FILELIST_BINARY.new >> $FILELIST_BINARY
    sort $FILELIST_BINARY | sort | uniq > $FILELIST_BINARY.new
    mv -f $FILELIST_BINARY.new $FILELIST_BINARY

    # find chains of symbolic links in /lib dir
    grep -E '^/lib/' $FILELIST_BINARY | xargs -I % -n 1 find -L /lib/ -samefile % -print >> $FILELIST_BINARY.new
    cat $FILELIST_BINARY.new >> $FILELIST_BINARY
    sort $FILELIST_BINARY | uniq > $FILELIST_BINARY.new
    mv -f $FILELIST_BINARY.new $FILELIST_BINARY
}

tar_files() {
    local TARLIST=/tmp/tarlist
    cat $FILELIST > $TARLIST
    cat $FILELIST_BINARY >> $TARLIST
    tar -czf $TMP_TAR --no-recursion -T $TARLIST
    rm -f $TARLIST
}

make_image_tar() {
    mkdir -p $BUILD_ROOT
    cd $BUILD_ROOT
    tar xzf $TMP_TAR
    /bin/busybox --install -s bin
    tar czf /usr/src/kazoo/pkg/alpine_docker/$IMG_TAR *
}

prepare_build

build_and_install

kazoo_files > $FILELIST
extra_files >> $FILELIST
sort_filelist
filter_unnecessary_files
find_binaries
tar_files
make_image_tar

exit 0
