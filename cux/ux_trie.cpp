#include <ux/ux.hpp>

#include <string>
#include <stdint.h>
#include <stdlib.h>

typedef void *Trie;

extern "C" {

static inline void decodeString(const std::string &str, char **ret, size_t *len)
{
  *len = str.length();
  *ret = reinterpret_cast<char*>(malloc(sizeof(char) * (*len)));
  for (size_t i = 0; i < *len; i++)
    (*ret)[i] = str[i];
}

static inline void decodeVector(const std::vector<uint64_t> &vect,
                                uint64_t **ret, size_t *len)
{
  *len = vect.size();
  *ret = reinterpret_cast<uint64_t*>(malloc(sizeof(uint64_t) * (*len)));
  for (size_t i = 0; i < *len; i++)
    (*ret)[i] = vect[i];
}

Trie newTrie()
{
  return new ux::Trie();
}

void deleteTrie(Trie p)
{
  delete reinterpret_cast<ux::Trie*>(p);
}

void buildTrie(Trie p, char **keyList, int keyNum, int isTailUX)
{
  std::vector<std::string> kl;
  kl.reserve(keyNum);
  for (int i = 0; i < keyNum; i++)
    kl.push_back(std::string(keyList[i]));
  reinterpret_cast<ux::Trie*>(p)->build(kl, isTailUX);
}

int saveTrie(Trie p, char *indexName)
{
  return reinterpret_cast<const ux::Trie*>(p)->save(indexName);
}

int loadTrie(Trie p, char *indexName)
{
  return reinterpret_cast<ux::Trie*>(p)->load(indexName);
}

uint64_t prefixSearchTrie(Trie p, char *str, size_t len, size_t *retLen)
{
  return reinterpret_cast<const ux::Trie*>(p)->prefixSearch(str, len, *retLen);
}

size_t commonPrefixSearchTrie(Trie p, char *str, size_t len,
                              uint64_t **retIDs, size_t *retNum, size_t limit)
{
  std::vector<ux::id_t> vect;
  size_t ret =
    reinterpret_cast<const ux::Trie*>(p)->commonPrefixSearch(str, len, vect, limit);
  decodeVector(vect, retIDs, retNum);
  return ret;
}

size_t predictiveSearchTrie(Trie p, char *str, size_t len,
                            uint64_t **retIDs, size_t *retNum, size_t limit)
{
  std::vector<ux::id_t> vect;
  size_t ret =
    reinterpret_cast<const ux::Trie*>(p)->predictiveSearch(str, len, vect, limit);
  decodeVector(vect, retIDs, retNum);
  return ret;
}

void decodeKeyTrie(Trie p, uint64_t id, char **ret, size_t *len)
{
  std::string str;
  reinterpret_cast<const ux::Trie*>(p)->decodeKey(id, str);
  decodeString(str, ret, len);
}

size_t sizeTrie(Trie p)
{
  return reinterpret_cast<const ux::Trie*>(p)->size();
}

void clearTrie(Trie p)
{
  reinterpret_cast<ux::Trie*>(p)->clear();
}

size_t getAllocSizeTrie(Trie p)
{
  return reinterpret_cast<const ux::Trie*>(p)->getAllocSize();
}

void whatTrie(int error, char **ret, size_t *len)
{
  std::string str = ux::Trie::what(error);
  decodeString(str, ret, len);
}

}
