/// Choose to verify, or skip verification for host.
pub type Verify {
  VerifyNone
  VerifyPeer
}

/// Options for SSL connection.
pub type Options {
  /// Verify or skip the peer.
  Verify(Verify)
  /// List of certificates to verify against.
  Cacerts(certs: List(BitArray))
  /// Filename of certificate to verify against.
  Cacertfile(filename: String)
}
