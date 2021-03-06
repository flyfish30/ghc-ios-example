LIB=SwiftHsLib

ARCHIVE=libHS${LIB}
GHC_TH_OPTIONS=-fexternal-interpreter -pgmi=${HOME}/.ghcup/bin/iserv-proxy -opti10.0.0.5 -opti5000

.PHONY: cabal-build
cabal-build:
	$(CABAL) new-configure --disable-shared --enable-static --allow-newer --ghc-option=-fllvm
	$(CABAL) new-build --allow-newer --ghc-option="-fllvm" # ${GHC_TH_OPTIONS}

binaries/iOS/$(ARCHIVE).a: src/*.hs
	CABAL=x86_64-apple-ios-cabal make cabal-build
	# CABAL=aarch64-apple-ios-cabal make cabal-build
	mkdir -p $(@D)
	find . -path "*-ios*" -name "${ARCHIVE}*ghc*.a" -exec lipo -create -output $@ {} +

binaries/macOS/$(ARCHIVE).a:
	CABAL=cabal make cabal-build
	mkdir -p $(@D)
	find . -path "*-osx*" -name "${ARCHIVE}*ghc*.a" -exec lipo -create -output $@ {} +

binaries/android/armv7a/${ARCHIVE}.a:
	CABAL=armv7-linux-androideabi-cabal make cabal-build
	mkdir -p $(@D)
	find . -path "*arm-*-android*" -name "${ARCHIVE}*ghc*.a" -exec cp {} $@ \;

binaries/android/arm64-v8a/${ARCHIVE}.a:
	CABAL=aarch64-linux-android-cabal make cabal-build
	mkdir -p $(@D)
	find . -path "*aarch64-*-android*" -name "${ARCHIVE}*ghc*.a" -exec cp {} $@ \;

.PHONY: iOS
.PHONY: macOS
.PHONY: android
.PHONY: all
.PHONY: clean
iOS: binaries/iOS/${ARCHIVE}.a
macOS: binaries/macOS/${ARCHIVE}.a
android: binaries/android/armv7a/${ARCHIVE}.a binaries/android/arm64-v8a/${ARCHIVE}.a
all: iOS macOS android
clean:
	rm -R binaries
