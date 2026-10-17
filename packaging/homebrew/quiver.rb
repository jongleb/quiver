class Quiver < Formula
  desc "Parallel consistent logical dumper for MySQL and MariaDB"
  homepage "https://github.com/jongleb/quiver"
  license "AGPL-3.0-or-later"

  on_macos do
    depends_on arch: :arm64

    on_arm do
      url "https://github.com/jongleb/quiver/releases/download/v@VERSION@/quiver-@VERSION@-aarch64-apple-darwin.tar.gz"
      sha256 "@SHA256:aarch64-apple-darwin@"
    end
  end

  on_linux do
    on_intel do
      url "https://github.com/jongleb/quiver/releases/download/v@VERSION@/quiver-@VERSION@-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "@SHA256:x86_64-unknown-linux-gnu@"
    end

    on_arm do
      url "https://github.com/jongleb/quiver/releases/download/v@VERSION@/quiver-@VERSION@-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "@SHA256:aarch64-unknown-linux-gnu@"
    end
  end

  def install
    bin.install "quiver"
    doc.install "README.md"
    (share/"licenses"/name).install "LICENSE", Dir["licenses/*"]
  end

  test do
    assert_equal version.to_s, shell_output("#{bin}/quiver --version").strip
  end
end
