FROM ubuntu:23.10

RUN apt update
RUN apt -y dist-upgrade
RUN apt -y install git git-lfs wget make

RUN wget https://packages.microsoft.com/config/ubuntu/23.10/packages-microsoft-prod.deb 
RUN dpkg -i packages-microsoft-prod.deb

RUN apt update
RUN apt -y install dotnet-sdk-8.0

USER ubuntu
