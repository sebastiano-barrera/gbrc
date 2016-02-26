"""
Extract Z80 opcode information from the HTML page fetched from
<http://imrannazar.com/Gameboy-Z80-Opcode-Map>, part of the excellent
blog post series by Imran Nazar.

Usage:
        extract.py <filename>
"""

from docopt import docopt
import bs4
import json

def parse_table(table):
    opcodes = {}

    rows = table.find_all('tr')
    header, rows = rows[0], rows[1:]
    low_nibbles = [cell.text for cell in header.find_all('th')[1:]]
    for row in rows:
        cells = row.find_all('td')
        high = cells[0].text
        for i, cell in enumerate(cells[1:]):
            low = low_nibbles[i]
            opcode = high.replace('x', low)

            toks = cell.text.split()
            assert len(toks) <= 2
            if len(toks) == 1:
                operands = []
            else:
                operands = toks[1].split(',')

            opcodes[opcode] = {
                'Desc': cell.abbr.attrs['title'],
                'Instr': toks[0],
                'Operands': operands
            }

    return opcodes


def main():
    args = docopt(__doc__)
    if '<filename>' not in args:
        print('Input file name required. Try -h')
        return

    with open(args['<filename>']) as input_file:
        soup = bs4.BeautifulSoup(input_file)
        plane_root, plane_CB = soup.find_all('table')
        opcodes = {
        	'Direct': parse_table(plane_root),
        	'CB': parse_table(plane_CB),
        }

    print(json.dumps(opcodes, indent=4))

if __name__ == '__main__':
    main()
