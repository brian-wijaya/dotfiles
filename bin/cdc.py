#!/usr/bin/env python3
"""
FastCDC (Fast Content-Defined Chunking) implementation.

Based on "FastCDC: a Fast and Efficient Content-Defined Chunking Approach"
by Wen Xia et al. (2016).

Provides content-defined chunking with normalization for improved deduplication.
"""

import hashlib
from typing import List, Tuple


class FastCDC:
    """Fast Content-Defined Chunking with gear hash and normalization."""

    # Gear hash lookup table (256 random 64-bit values)
    # Pre-computed for performance
    GEAR_HASH_TABLE = [
        0x5c95c078, 0x22408989, 0x2d48a214, 0x12842087, 0x530f8afb, 0x474536b9, 0x2963b4f1, 0x44cb738b,
        0x4ea7403d, 0x4d606b6e, 0x074ec5d3, 0x3af39d18, 0x726003ca, 0x37a62a74, 0x51a2f58e, 0x7506358e,
        0x5d4ab128, 0x4d4ae17b, 0x41e85924, 0x470c36f7, 0x4741cbe1, 0x01bb7f30, 0x617c1de3, 0x2b0c3a1f,
        0x50c48f73, 0x21a82d37, 0x6095ace0, 0x419167a0, 0x3caf49b0, 0x40cea62d, 0x66bc1c66, 0x545e1dad,
        0x2bfa77cd, 0x6e85da24, 0x5fb0bdc5, 0x652cfc29, 0x3a0ae1ab, 0x2837e0f3, 0x6387b70e, 0x01e2e3ee,
        0x0e3a30ac, 0x12fac046, 0x40f9a4c2, 0x14167cf4, 0x7bb31f3c, 0x1a5a0be0, 0x1f32dc3b, 0x40065c0f,
        0x0d0d8f9e, 0x6e8fcf10, 0x4e08ca69, 0x2f73f21e, 0x4a30c3f6, 0x2cf9b30b, 0x4f3d8963, 0x5d42f0ed,
        0x2d3e6be3, 0x049e9958, 0x3c2f5d59, 0x75f5e394, 0x1d68f024, 0x23a45b7b, 0x6084b08e, 0x034aba52,
        0x0e68e5db, 0x4ea7b602, 0x32f19151, 0x4bdcc71a, 0x3d1e93ce, 0x48b697ab, 0x52012a18, 0x20ec7616,
        0x210a1736, 0x6f496a79, 0x71cf5fa6, 0x0b6e9dce, 0x62a12cef, 0x4c0f0f6f, 0x7aabf26e, 0x4ad42d39,
        0x35e8f85b, 0x265feeae, 0x1e2f95c8, 0x5f48f7a1, 0x0bb82fe7, 0x5e1c4207, 0x1e2c5c27, 0x3c56b8a2,
        0x6d6d3f1a, 0x70e5f18b, 0x0a87e433, 0x1c8f2c2b, 0x58f8a8fc, 0x04b82c6c, 0x20bcf6bb, 0x6ac1794c,
        0x64030f64, 0x14eb3fa1, 0x44dfc652, 0x6adb1f8e, 0x437c1b2e, 0x13c45fe1, 0x0e8e5f88, 0x0d8e0fcf,
        0x456d9d1a, 0x6bdc9ef1, 0x07901cf7, 0x03c0bfdc, 0x4a5aa62e, 0x34c2f2c9, 0x6f37e09d, 0x57e7e6ec,
        0x5b0d3c8c, 0x13e78691, 0x5e2d8e67, 0x63bfcd15, 0x1ff37330, 0x297ac56a, 0x52af7869, 0x5e74cb0e,
        0x7e5b6534, 0x5fb9c586, 0x2a39f03e, 0x2b0a6f22, 0x1e3a7f2e, 0x48f9d11e, 0x10a8c863, 0x5d5b2c8b,
        0x3e98cb0d, 0x02b47e0e, 0x4a1c04ae, 0x0c85e37a, 0x5a33ee7b, 0x2cf44b8a, 0x0af1e8f6, 0x4e78c3b8,
        0x28f8b12c, 0x546fa3c7, 0x13c3f6f9, 0x2d4c2a6e, 0x0876c4b0, 0x0f9e2a6f, 0x36d22d7b, 0x535d36e7,
        0x74cd48c6, 0x57a48cf6, 0x73d2b6dc, 0x20b8aa8a, 0x5dbf4cd1, 0x3cd74aa2, 0x5ab8934f, 0x48d0c7e3,
        0x67f3d5bb, 0x6f42e8d0, 0x0e46af98, 0x65bf7f67, 0x1b05b8c2, 0x40825cb9, 0x6bae3e4c, 0x5e4b3e5a,
        0x2ee2a5e8, 0x0f7e4e96, 0x7a19e3f5, 0x17bf7e46, 0x5e85c6db, 0x69a0a1e9, 0x11d30d4e, 0x73dbc6e3,
        0x4b43d4e9, 0x1fcab0dd, 0x5cb7f87f, 0x0c1db6f9, 0x51d8d8f6, 0x5cd8e11f, 0x7eab3a8c, 0x6456b9e6,
        0x7e391be1, 0x5e8d8f14, 0x1f79fe59, 0x53d1f5d2, 0x4e9e1ea4, 0x6f9e07ba, 0x0c1e8a6a, 0x3c7a7a1a,
        0x1b0f7fc0, 0x1e6b4d73, 0x4c8a8c72, 0x65f6e1e0, 0x2e8f87e6, 0x66f7c6ea, 0x77d9e8e3, 0x0a8f8e3c,
        0x48bfc7e8, 0x54e9e6c7, 0x6f9f1e3c, 0x5c8f8e1a, 0x72e1e6f8, 0x3e8f9c1a, 0x6f8e7e3a, 0x4e9f1e6c,
        0x5c8a9e3f, 0x7e9f8c1a, 0x3f9e1e8c, 0x6c8f7a3e, 0x5e1f9c8a, 0x7a8e3f1c, 0x4c9f1e8a, 0x6e8a3f7c,
        0x5f1c9e8a, 0x7c3a8f1e, 0x4e1a9f8c, 0x6a8c3e7f, 0x5c1e8f9a, 0x7e3f1c8a, 0x4f8a1e9c, 0x6c7e3a8f,
        0x5e9c1f8a, 0x7a8f3e1c, 0x4c1e9a8f, 0x6e3f8c7a, 0x5f8a1c9e, 0x7c1e8a3f, 0x4e9f8c1a, 0x6a7c3e8f,
        0x5c8e1f9a, 0x7e3a8f1c, 0x4f1c9e8a, 0x6c8a7e3f, 0x5e1f8c9a, 0x7a3e8f1c, 0x4c9a1e8f, 0x6e8c3f7a,
        0x5f1a9c8e, 0x7c8e3a1f, 0x4e1f8a9c, 0x6a3e7c8f, 0x5c9e1f8a, 0x7e8f3c1a, 0x4f8c1e9a, 0x6c3a7e8f,
        0x5e1c8f9a, 0x7a8e1f3c, 0x4c9f8a1e, 0x6e7c3f8a, 0x5f8e1c9a, 0x7c1a8e3f, 0x4e8f9c1a, 0x6a3f7c8e,
        0x5c1f8e9a, 0x7e8a3f1c, 0x4f9c1e8a, 0x6c7a8e3f, 0x5e8f1c9a, 0x7a1e8f3c, 0x4c8a9f1e, 0x6e3c8f7a
    ]

    def __init__(
        self,
        min_size: int = 8 * 1024,      # 8KB
        avg_size: int = 16 * 1024,     # 16KB (target size)
        max_size: int = 32 * 1024,     # 32KB
        normalization: int = 2          # Normalization level (1, 2, or 3)
    ):
        """Initialize FastCDC with chunk size parameters.

        Args:
            min_size: Minimum chunk size in bytes
            avg_size: Average (target) chunk size in bytes
            max_size: Maximum chunk size in bytes
            normalization: Normalization level (higher = better dedup, slower)
        """
        self.min_size = min_size
        self.avg_size = avg_size
        self.max_size = max_size
        self.normalization = normalization

        # Calculate masks for cut point detection
        # Mask bits = log2(avg_size) + normalization
        mask_bits = self._log2(avg_size) + normalization
        self.mask = (1 << mask_bits) - 1

        # Masks for different regions (FastCDC optimization)
        self.mask_s = (1 << (mask_bits + 1)) - 1  # Small chunk region
        self.mask_l = (1 << (mask_bits - 1)) - 1  # Large chunk region

    @staticmethod
    def _log2(n: int) -> int:
        """Calculate log2 of n."""
        return n.bit_length() - 1

    def _gear_hash(self, data: bytes, start: int, end: int) -> int:
        """Calculate gear hash for byte range.

        Args:
            data: Input data
            start: Start index
            end: End index (exclusive)

        Returns:
            Rolling hash value
        """
        hash_val = 0
        for i in range(start, min(end, len(data))):
            hash_val = ((hash_val << 1) + self.GEAR_HASH_TABLE[data[i]]) & 0xFFFFFFFFFFFFFFFF
        return hash_val

    def chunk(self, data: bytes) -> List[Tuple[str, int, int]]:
        """Chunk data using FastCDC algorithm.

        Args:
            data: Input data to chunk

        Returns:
            List of tuples (chunk_hash, offset, length)
        """
        if not data:
            return []

        chunks = []
        data_len = len(data)
        pos = 0

        while pos < data_len:
            chunk_start = pos
            chunk_end = min(pos + self.max_size, data_len)

            # Minimum chunk size - no cut point detection
            if pos + self.min_size >= data_len:
                # Last chunk
                chunk_data = data[pos:]
                chunk_hash = hashlib.sha256(chunk_data).hexdigest()
                chunks.append((chunk_hash, pos, len(chunk_data)))
                break

            # Find cut point using gear hash
            cut_point = self._find_cut_point(data, pos)

            if cut_point is None:
                # No cut point found, use max size
                cut_point = chunk_end

            # Extract chunk
            chunk_data = data[pos:cut_point]
            chunk_hash = hashlib.sha256(chunk_data).hexdigest()
            chunks.append((chunk_hash, pos, len(chunk_data)))

            pos = cut_point

        return chunks

    def _find_cut_point(self, data: bytes, start: int) -> int:
        """Find cut point using FastCDC gear hash.

        Args:
            data: Input data
            start: Start position

        Returns:
            Cut point position, or None if max_size reached
        """
        data_len = len(data)
        min_pos = start + self.min_size
        max_pos = min(start + self.max_size, data_len)
        avg_pos = start + self.avg_size

        # Initialize rolling hash
        hash_val = 0

        # Three regions with different masks for better distribution:
        # 1. [min, avg/2): Use mask_s (smaller chunks)
        # 2. [avg/2, avg*2): Use mask (normal chunks)
        # 3. [avg*2, max): Use mask_l (larger chunks)

        pos = min_pos

        # Region 1: Before average size / 2 - use stricter mask (prefer small)
        region1_end = min(avg_pos // 2 if avg_pos > start else max_pos, max_pos)
        while pos < region1_end:
            hash_val = ((hash_val << 1) + self.GEAR_HASH_TABLE[data[pos]]) & 0xFFFFFFFFFFFFFFFF
            if (hash_val & self.mask_s) == 0:
                return pos + 1
            pos += 1

        # Region 2: Around average size - use normal mask
        region2_end = min(avg_pos * 2, max_pos)
        while pos < region2_end:
            hash_val = ((hash_val << 1) + self.GEAR_HASH_TABLE[data[pos]]) & 0xFFFFFFFFFFFFFFFF
            if (hash_val & self.mask) == 0:
                return pos + 1
            pos += 1

        # Region 3: Near max size - use lenient mask (prefer large)
        while pos < max_pos:
            hash_val = ((hash_val << 1) + self.GEAR_HASH_TABLE[data[pos]]) & 0xFFFFFFFFFFFFFFFF
            if (hash_val & self.mask_l) == 0:
                return pos + 1
            pos += 1

        # Reached max size
        return max_pos


def chunk_file(file_path: str, **kwargs) -> List[Tuple[str, int, int]]:
    """Convenience function to chunk a file.

    Args:
        file_path: Path to file to chunk
        **kwargs: Arguments to pass to FastCDC constructor

    Returns:
        List of tuples (chunk_hash, offset, length)
    """
    with open(file_path, 'rb') as f:
        data = f.read()

    cdc = FastCDC(**kwargs)
    return cdc.chunk(data)


if __name__ == "__main__":
    # Simple test
    import sys

    if len(sys.argv) < 2:
        print("Usage: cdc.py <file>")
        sys.exit(1)

    file_path = sys.argv[1]
    chunks = chunk_file(file_path)

    print(f"File: {file_path}")
    print(f"Chunks: {len(chunks)}")
    print(f"Total size: {sum(length for _, _, length in chunks)} bytes")
    print(f"\nChunk details:")
    for i, (chunk_hash, offset, length) in enumerate(chunks):
        print(f"  {i+1}. {chunk_hash[:16]}... offset={offset} length={length}")
