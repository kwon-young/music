import argparse
from typing import Optional
from pathlib import Path as PLPath
import xml.etree.ElementTree as ET
from dataclasses import dataclass
import json
import io

from svgelements import Length, Rect, Viewbox, Matrix, Path, Point, Ellipse, \
    Polygon, Group
import cairosvg
from PIL import Image
import numpy as np
from pycocotools import mask


def make_args():
    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('svg', type=PLPath, help='svg file to parse')
    parser.add_argument('glyphnames', type=PLPath, help='glyphnames.json file')
    parser.add_argument('output', type=PLPath, help='output file')

    args = parser.parse_args()
    return args


def pointtopl(point: Optional[Point]):
    if point:
        return f"point({point.x}, {point.y})"
    else:
        return "_"


def listtopl(els, sep=' ', start='', end=''):
    return f"[{start}" + f",{sep}".join([x.topl() for x in els]) + f"{end}]"


@dataclass
class IdClass:
    id: Optional[str]
    c: str

    def topl(self):
        if self.id:
            if isinstance(self.id, int):
                id = f"{self.id}"
            else:
                id = f"'{self.id}'"
        else:
            id = '_'
        return f"{id}-'{self.c}'"


def dtorle(d, x, y, width, height, parent_width, parent_height):
    g = Group()
    g.append(Path(d=d, fill='white', x=-x, y=-y))
    xml = g.string_xml()
    img = cairosvg.svg2png(
        xml, parent_width=width,
        parent_height=height)
    img = Image.open(io.BytesIO(img)).convert('L')
    arr = np.array(img)
    background = np.zeros([parent_height, parent_width], dtype=np.uint8)
    background[round(y):round(y)+arr.shape[0],
               round(x):round(x)+arr.shape[1]] = arr
    rs = segmentationToCocoMask(background, 255)
    return rs


@dataclass
class Seg:
    d: str | None
    start: Point
    end: Point
    etiq: list
    thickness: float

    def topl(self):
        return f"seg({pointtopl(self.start)}, {pointtopl(self.end)}, " \
                f"{listtopl(self.etiq[::-1])}, {self.thickness})"

    def bbox(self):
        delta = self.thickness / 2
        x = min(self.start.x, self.end.x) - delta
        xmax = max(self.start.x, self.end.x) + delta
        width = xmax - x
        y = min(self.start.y, self.end.y) - delta
        ymax = max(self.start.y, self.end.y) + delta
        height = ymax - y
        return (x, y, width, height)

    def tococo(self, parent_width, parent_height):
        bbox = self.bbox()
        rle = dtorle(self.d, *bbox, parent_width, parent_height)
        _, _, width, height = bbox
        area = width * height
        return {
            'category': 'seg',
            'segmentation': rle,
            'area': area,
            'bbox': bbox,
            'iscrowd': 0,
            'keypoints': [self.start.x, self.start.y, 2,
                          self.end.x, self.end.y, 2],
            'num_keypoints': 2,
        }


@dataclass
class Ccx:
    d: str
    lefttop: Point
    rightbottom: Point
    etiq: list[tuple[Optional[str], str]]
    origin: Point = None

    def topl(self):
        return f"ccx({pointtopl(self.lefttop)}, " \
               f"{pointtopl(self.rightbottom)}, " \
               f"{listtopl(self.etiq[::-1])}, " \
               f"{pointtopl(self.origin)})"

    def bbox(self):
        x, y = self.lefttop.x, self.lefttop.y
        xmax, ymax = self.rightbottom.x, self.rightbottom.y
        width, height = xmax - x, ymax - y
        return x, y, width, height

    def tococo(self, parent_width, parent_height):
        bbox = self.bbox()
        rle = dtorle(self.d, *bbox, parent_width, parent_height)
        _, _, width, height = bbox
        area = width * height
        return {
            'category': self.etiq[-1].c,
            'segmentation': rle,
            'area': area,
            'bbox': bbox,
            'iscrowd': 0,
            'keypoints': [self.origin.x, self.origin.y, 2],
            'num_keypoints': 1,
        }


ns = {'svg': "{http://www.w3.org/2000/svg}",
      'xlink': "{http://www.w3.org/1999/xlink}"}


def get_tag(node):
    ntag = node.tag
    for k, v in ns.items():
        if v in ntag:
            ntag = ntag.replace(v, k + ':')
    return ntag


def get_attrib(node, attrib):
    name, attrib = attrib.split(':')
    link = ns[name]
    attrib = link + attrib
    return node.attrib[attrib]


def get_viewport(transforms):
    for transform in transforms:
        if isinstance(transform, Rect):
            return transform


def apply_transforms(obj, transforms):
    inv_trans = transforms[::-1]
    for i, transform in enumerate(inv_trans):
        if isinstance(transform, Matrix):
            obj = obj * transform
        elif isinstance(transform, Viewbox):
            viewport = get_viewport(inv_trans[i + 1:])
            obj = obj * transform.transform(viewport)
    return obj


def recurse(f):
    def wrapper(node, *args):
        node_res = f(node, *args)
        res = []
        if node_res is not None:
            res.append(node_res)
        for child in node:
            child_res = parse_node(child, *args)
            if isinstance(child_res, list):
                res.extend(child_res)
            elif child_res is not None:
                res.append(child_res)
        return res
    return wrapper


def backtrack(f):
    def wrapper(node, *args):
        args_pre = []
        for arg in args:
            if isinstance(arg, list):
                args_pre.append(len(arg))
            elif isinstance(arg, dict):
                args_pre.append(set(arg.keys()))
        res = f(node, *args)
        for arg, arg_pre in zip(args, args_pre):
            if isinstance(arg, list):
                assert len(arg) >= arg_pre
                for _ in range(len(arg) - arg_pre):
                    arg.pop(-1)
            elif isinstance(arg, dict):
                assert arg_pre.issubset(set(arg.keys()))
                for k in set(arg.keys()) - arg_pre:
                    del arg[k]
        return res
    return wrapper


@backtrack
@recurse
def parse_svgnode(node, transforms, defs, scopes):
    res = None
    if w := node.attrib.get('width'):
        if h := node.attrib.get('height'):
            # scopes.append(IdClass(None, 'page'))
            width = Length(w)
            height = Length(h)
            transforms.append(Rect(0, 0, width, height))
            res = Ccx(None, Point(0, 0), Point(width.value(), height.value()),
                      scopes.copy(), Point(0, 0))
    if viewBox := node.attrib.get('viewBox'):
        transforms.append(Viewbox(viewBox))
    return res


def parse_defs(node, transforms, defs, scopes):
    for child in node:
        defs[child.attrib['id']] = child
    return


@backtrack
@recurse
def parse_g(node, transforms, defs, scopes):
    if transform := node.attrib.get('transform'):
        transforms.append(Matrix(transform))
    if gclass := node.attrib.get('class'):
        if "ledgerLines" in gclass:
            gclass = gclass.split(' ')[0]
        node_id = node.attrib.get('id', None)
        for scope in scopes:
            if gclass == scope.c and node_id is None:
                break
        else:
            scopes.append(IdClass(node_id, gclass))


def seg_swap(direction, start, end):
    if direction == 'h' and start.x > end.x:
        start, end = end, start
    if direction == 'v' and start.y > end.y:
        start, end = end, start
    return start, end


def poly_swap(direction, points):
    if direction == 'h':
        return sorted(points, key=lambda p: (p.x, p.y))
    if direction == 'v':
        return sorted(points, key=lambda p: (p.y, p.x))


def segmentationToCocoMask(labelMap, labelId):
    '''
    Encodes a segmentation mask using the Mask API.
    :param labelMap: [h x w] segmentation map that indicates the label of each pixel
    :param labelId: the label from labelMap that will be encoded
    :return: Rs - the encoded label mask for label 'labelId'
    '''
    labelMask = labelMap == labelId
    labelMask = np.expand_dims(labelMask, axis=2)
    labelMask = labelMask.astype('uint8')
    labelMask = np.asfortranarray(labelMask)
    Rs = mask.encode(labelMask)
    assert len(Rs) == 1
    Rs = Rs[0]

    return Rs


@backtrack
def parse_path(node, transforms, defs, scopes):
    p = Path(**node.attrib)
    p = apply_transforms(p, transforms)
    p.reify()
    h_lines = ['staff', 'ledgerLines']
    v_lines = ['barLine', 'system', 'stem']
    if scopes and scopes[-1].c in h_lines + v_lines:
        points = [point for point in p.as_points()]
        if scopes[-1].c in h_lines:
            start, end = seg_swap('h', points[0], points[-1])
        elif scopes[-1].c in v_lines:
            start, end = seg_swap('v', points[0], points[-1])
        return Seg(p.d(), start, end, scopes.copy(), p.stroke_width)
    elif scopes and scopes[-1].c == 'beam':
        p1, p2, width = poly_to_hseg(p.as_points())
        return Seg(p.d(), p1, p2, scopes.copy(), width)
    else:
        left, top, right, bottom = p.bbox()
        origin = Point(float(node.attrib.get('x', left)),
                       float(node.attrib.get('y', top)))
        ccx_scopes = scopes.copy()
        if label := node.attrib.get('class', None):
            ccx_scopes.append(IdClass(p.id, label))
        return Ccx(p.d(), Point(left, top), Point(right, bottom), ccx_scopes,
                   origin)


@backtrack
def parse_use(node, transforms, defs, scopes):
    attrib = node.attrib
    x = Length(attrib['x'])
    y = Length(attrib['y'])
    if 'transform' in attrib:
        transforms.append(Matrix(attrib['transform']))
    origin = apply_transforms(Point(x, y), transforms)
    transforms.append(Matrix.translate(x, y))
    if w := attrib.get('width'):
        if h := attrib.get('height'):
            transforms.append(Rect(0, 0, Length(w), Length(h)))
    href = get_attrib(node, 'xlink:href')
    symbol = defs[href.replace('#', '')]
    [ccx] = parse_node(symbol, transforms, defs, scopes)
    ccx.origin = origin
    glyphcode = href.split('-')[0]
    ccx.etiq.append(IdClass(node.attrib.get('id', None), glyphcode))
    return [ccx]


@backtrack
@recurse
def parse_symbol(node, transforms, defs, scopes):
    if viewBox := node.attrib.get('viewBox'):
        transforms.append(Viewbox(viewBox))


def parse_rect(node, transforms, defs, scopes):
    r = Rect(**node.attrib)
    r = apply_transforms(r, transforms)
    r.reify()
    if scopes[-1].c in ['stem', 'grpSym']:
        x, y, w, h = r.x, r.y, r.width, r.height
        x = x + (w / 2)
        return Seg(r.d(), Point(x, y), Point(x, y + h), scopes.copy(), w)


def parse_ellipse(node, transforms, defs, scopes):
    e = Ellipse(**node.attrib)
    e = apply_transforms(e, transforms)
    e.reify()
    if scopes[-1].c == 'dots':
        left, top, right, bottom = e.bbox()
        return Ccx(e.d(), Point(left, top), Point(right, bottom),
                   scopes.copy(), Point(e.cx, e.cy))


def parse_polygon(node, transforms, defs, scopes):
    r = Polygon(**node.attrib)
    r = apply_transforms(r, transforms)
    r.reify()
    if scopes[-1].c == 'beam':
        p1, p2, width = poly_to_hseg(r.points)
        return Seg(r.d(), p1, p2, scopes.copy(), width)


def poly_to_hseg(points):
    lefttop, leftbottom, righttop, rightbottom = sorted(
        set(points), key=lambda p: (p.x, p.y))
    p1 = Point((lefttop.x + leftbottom.x) / 2,
               (lefttop.y + leftbottom.y) / 2)
    p2 = Point((righttop.x + rightbottom.x) / 2,
               (righttop.y + rightbottom.y) / 2)
    width = abs(leftbottom.y - lefttop.y)
    return p1, p2, width


# def parse_text(node, transforms, defs, scopes):
#     __import__('ipdb').set_trace()
#     return


def parse_node(node, transforms, defs, scopes):
    tag = get_tag(node)
    res = None
    if tag == "svg:svg":
        res = parse_svgnode(node, transforms, defs, scopes)
    elif tag == "svg:defs":
        res = parse_defs(node, transforms, defs, scopes)
    elif tag == "svg:g" and node.attrib.get('class') != 'pgFoot autogenerated':
        res = parse_g(node, transforms, defs, scopes)
    elif tag == "svg:path":
        res = parse_path(node, transforms, defs, scopes)
    elif tag == "svg:use":
        res = parse_use(node, transforms, defs, scopes)
    elif tag == "svg:symbol":
        res = parse_symbol(node, transforms, defs, scopes)
    elif tag == "svg:rect":
        res = parse_rect(node, transforms, defs, scopes)
    elif tag == "svg:ellipse":
        res = parse_ellipse(node, transforms, defs, scopes)
    elif tag == "svg:polygon":
        res = parse_polygon(node, transforms, defs, scopes)
    # elif tag == "svg:text":
    #     res = parse_text(node, transforms, defs, scopes)
    return res


def sort_elements(element):
    if isinstance(element, Seg):
        p = element.start
    elif isinstance(element, Ccx):
        p = element.origin
    x = p.x.value() if isinstance(p.x, Length) else p.x
    y = p.y.value() if isinstance(p.y, Length) else p.y
    return (x, y)


def load_glyphnames(path: Path) -> dict[str, str]:
    with path.open() as f:
        glyphnames = json.load(f)
    glyphnames_inv = {
        v['codepoint']: {'name': k, 'description': v['description']}
        for k, v in glyphnames.items()
    }
    return glyphnames_inv


def main(args):
    page_number = int(args.svg.stem.split('_')[-1])
    tree = ET.parse(args.svg)
    root = tree.getroot()
    res = parse_node(root, [], {}, [IdClass(page_number, 'page')])
    glyphnames_inv = load_glyphnames(args.glyphnames)
    for el in res:
        for i in range(len(el.etiq)):
            if el.etiq[i].c.startswith('#'):
                codepoint = el.etiq[i].c.replace('#', 'U+')
                el.etiq[i] = IdClass(el.etiq[i].id,
                                     glyphnames_inv[codepoint]['name'])
    res.sort(key=sort_elements)
    # __import__('pprint').pprint(res)
    with args.output.open('w') as f:
        f.write(listtopl(res, '\n\t', '\n\t', '\n') + '.')
    # for el in res[1:]:
    #     __import__('pprint').pprint(el.tococo(5568, 7326))
    return


if __name__ == "__main__":
    main(make_args())
