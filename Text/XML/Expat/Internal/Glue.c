#include <expat.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    size_t offset;
    size_t capacity;
    uint8_t* block;
} Block;

/*!
 * Allocate the specified amount of room in the buffer.
 */
static void* alloc(Block* blk, size_t room)
{
    size_t minCapacity = blk->offset + room;
    size_t start = blk->offset;
    {
        int changed = 0;
        size_t capacity = blk->capacity;
        while (capacity < minCapacity) {
            capacity = capacity * 2;
            changed = 1;
        }
        if (changed) {
            blk->capacity = capacity;
            blk->block = realloc(blk->block, capacity);
        }
    }
    blk->offset += room;
    return blk->block + start;
}

#define ROUND_UP_32(x) (((x) + 3) & ~3)

static void startElementHandler(
    void *userData,
    const XML_Char *name,
    const XML_Char **atts)
{
    Block* blk = (Block*)userData;
    size_t nameLen = strlen(name) + 1;
    size_t nAtts, i;
    for (nAtts = 0; atts[nAtts] != NULL; nAtts += 2) ;
    {
        uint32_t* hdr = alloc(blk, 8);
        hdr[0] = 1;
        hdr[1] = nAtts;
    }
    memcpy(alloc(blk, nameLen), name, nameLen);
    for (i = 0; i < nAtts; i++) {
        size_t attLen = strlen(atts[i]) + 1;
        memcpy(alloc(blk, attLen), atts[i], attLen);
    }
    blk->offset = ROUND_UP_32(blk->offset);
}

static void endElementHandler(void *userData, const XML_Char *name)
{
    Block* blk = (Block*)userData;
    size_t nameLen = strlen(name) + 1;
    *(uint32_t*)alloc(blk, 4) = 2;
    memcpy(alloc(blk, nameLen), name, nameLen);
    blk->offset = ROUND_UP_32(blk->offset);
}

static void characterDataHandler(
    void *userData,
    const XML_Char *s,
    int len)
{
    Block* blk = (Block*)userData;
    uint32_t* hdr = alloc(blk, 8);
    hdr[0] = 3;
    hdr[1] = len;
    memcpy(alloc(blk, (size_t)len), s, (size_t)len);
    blk->offset = ROUND_UP_32(blk->offset);
}

XML_Parser hexpatNewParser(const XML_Char* encoding)
{
    XML_Parser p = XML_ParserCreate(encoding);
    XML_SetStartElementHandler(p, startElementHandler);
    XML_SetEndElementHandler(p, endElementHandler);
    XML_SetCharacterDataHandler(p, characterDataHandler);
}

enum XML_Status hexpatParse(
    XML_Parser p,
    const char* s,
    int len,
    int isFinal,
    uint8_t** buffer,
    int* length)
{
    enum XML_Status ret;
    Block blk;
    blk.offset = 0;
    blk.capacity = 256;
    blk.block = malloc(blk.capacity);
    XML_SetUserData(p, &blk);
    ret = XML_Parse(p, s, len, isFinal);
    *(uint32_t*)alloc(&blk, 4) = 0;
    *buffer = blk.block;
    *length = (int)blk.offset;
    return ret;
}

