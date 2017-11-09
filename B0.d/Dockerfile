FROM ocaml/opam2:alpine-3.5-ocaml-4.05.0
ENV OPAMYES 1
ENV PATH="/home/opam/.opam/4.05.0/bin:${PATH}"
RUN sudo apk add m4
RUN opam pin add http://erratique.ch/repos/b0.git
CMD ["bash"]