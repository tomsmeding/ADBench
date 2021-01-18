# Using gcc:7.4.0 as base image
FROM gcc:7.4.0

# Install linux packages
# Debian bullseye gives libffi7
# LLVM's repo gives llvm-9-dev
# Accelerate (Haskell) needs libff7 and llvm-9-dev
# We install libff7 separately because the bullseye source overrides lots of stuff that we want to pull from buster.
RUN echo 'deb http://apt.llvm.org/buster/ llvm-toolchain-buster-9 main' >/etc/apt/sources.list.d/llvm-toolchain-buster-9.list \
    && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        wget=1.20.1-1.1 \
        python3=3.7.3-1 \
        python3-pip=18.1-5 \
        cmake=3.13.4-1 \
        julia=1.0.3+dfsg-4 \
        llvm-9-dev \
    && echo 'deb http://deb.debian.org/debian bullseye main' >/etc/apt/sources.list.d/debian-bullseye.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        libffi7=3.3-5 \
    && rm -rf /var/lib/apt/lists/*

# Install powershell
WORKDIR /utils/powershell
RUN wget -q https://github.com/PowerShell/PowerShell/releases/download/v6.2.3/powershell-6.2.3-linux-x64.tar.gz \
    && tar -xzf powershell-6.2.3-linux-x64.tar.gz \
    # Create a symlink to pwsh
    && ln -s /utils/powershell/pwsh /usr/local/bin \
    && rm powershell-6.2.3-linux-x64.tar.gz

# Install dotnet 2.1
RUN wget -q https://dot.net/v1/dotnet-install.sh \
    && chmod +x dotnet-install.sh \
    && ./dotnet-install.sh -c 2.1 \
    # Create a symlink to dotnet
    && ln -s ~/.dotnet/dotnet /usr/local/bin

# upgrade pip to be sure that tf>=2.0 could be installed
RUN python3 -m pip install --upgrade pip

# Module for python packages installing
RUN python3 -m pip install pip setuptools>=41.0.0

# Install stack (for Accelerate)
RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /adb
# Copy code to /adb (.dockerignore exclude some files)
COPY . .

# Setting workdir for building the project
WORKDIR /adb/build

# Configure and build
RUN cmake -DCMAKE_BUILD_TYPE=release .. \
    && make

WORKDIR /adb/ADBench
RUN sed -i 's/\r//' run-wrapper.sh \
    # make wrapper script executable
    && chmod +x run-wrapper.sh

ENTRYPOINT ["./run-wrapper.sh"]
