FROM gentoo/portage:latest as portage
FROM gentoo/stage3:latest

# copy the entire portage volume in
COPY --from=portage /var/db/repos/gentoo /var/db/repos/gentoo

COPY ./config/package.use /etc/portage/
COPY ./config/package.mask /etc/portage/
COPY ./config/package.accept_keywords /etc/portage/
COPY ./config/sets/paludis-deps /etc/portage/sets/

RUN emerge --update --newuse --deep -v @world \
    && emerge --update --newuse --deep -v @paludis-deps \
    && rm -rf /var/db/repos/*

# Unprivileged user
RUN useradd -M builder
USER builder
