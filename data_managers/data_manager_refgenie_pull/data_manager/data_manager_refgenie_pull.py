#!/usr/bin/env python


import argparse
import logging
from base64 import urlsafe_b64encode
from html import escape
from urllib.parse import urljoin

import refgenconf
import requests


log = logging.getLogger("tools.iuc.data_managers.data_manager_refgenie_pull")


def galaxy_code_get_refgenie_assets(refgenie_config_file):
    try:
        rval = []
        rgc = refgenconf.RefGenConf(refgenie_config_file, writable=False, skip_read_lock=True)
        for urlname, genomes in rgc.listr().items():
            urlname_64 = urlsafe_b64encode(bytes(urlname, 'utf8')).decode('utf8')
            ul = []
            for genome, assets in genomes.items():
                al = []
                for name in assets:
                    al.append({'name': name, 'value': '%s/%s/%s' % (urlname_64, genome, name), 'options': [], 'selected': False})
                ul.append({'name': genome, 'value': genome, 'options': al, 'selected': False})
            rval.append({'name': urlname, 'value': urlname_64, 'options': ul, 'selected': False})
        return rval
    except Exception as e:
        log.debug("Failed to access refgenie server: %s", e)
        return[{'name': escape(str(e)), 'value': 'ERROR', 'options': [], 'selected': False}]


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--names', dest='names', action='store', default=None, help='Table names to reload')
    parser.add_argument('-u', '--url', dest='url', action='store', default=None, help='Base url for reload')
    parser.add_argument('-k', '--key', dest='key', action='store', default=None, help='Galaxy API Key')
    parser.add_argument('-g', '--graceful', dest='graceful', action='store_true', help='Fail gracefully')

    args = parser.parse_args()
    try:
        if not args.names:
            tables = requests.get(urljoin(args.url, "api/tool_data"), params={'key': args.key}).json()
            args.names = [d.get('name') for d in tables]
        for name in args.names:
            print(requests.get(urljoin(args.url, "api/tool_data/%s/reload" % (name)), params={'key': args.key}).json())
    except Exception as e:
        if args.graceful:
            print("Failed to reload data tables:\n%s" % (e))
        else:
            raise e
